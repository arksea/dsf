package dsf.core;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author arksea
 */
public abstract class MessageChildTask<T> extends ChildTask<T> {

    private enum MessageType {

        CALL, INFO
    };

    private class TypedMessage {

        public TypedMessage(MessageType type, Message message, String from) {
            this.type = type;
            this.message = message;
            this.from = from;
            this.result = null;
        }

        public TypedMessage(MessageType type, Message message, String from, Result result) {
            this.type = type;
            this.message = message;
            this.from = from;
            this.result = result;
        }
        public final MessageType type;
        public final Message message;
        public final String from;
        public final Result result;
    };
    private final static Logger logger = LogManager.getLogger(Thread.class.getName());
    //stopInfo: 退出请求信息，
    /*
     * 为了避免某些JVM在volatile上实现的缺陷，只使用一个volatile变量进行退出判断。
     * 当stopInfo等于null时认为没有退出请求；
     * 当stopInfo等于"normal"时认为是一个正常退出的请求；
     * 当stopInfo等于其他值时，认为是因为错误而收到的退出请求，stopInfo的值即为错误信息。
     */
    private volatile String stopInfo = null;
    private long maxQueueLen;
    private final LinkedBlockingQueue<TypedMessage> queue = new LinkedBlockingQueue<TypedMessage>();

    //声明Throwable异常意味着任务可以抛出任何自己无法处理的异常，交由框架统一处理：记录日志并重启任务
    protected abstract void handle_info(Message msg, String from) throws Throwable;

    protected abstract Message handle_call(Message msg, String from) throws Throwable;

    public MessageChildTask(String name, long maxQueueLen, T args) {
        super(name, args);
        this.maxQueueLen = maxQueueLen;
    }

    private void tack(List<TypedMessage> msgList) throws InterruptedException {
        TypedMessage tmsg = queue.take();
        msgList.add(tmsg);
        queue.drainTo(msgList);
    }

    private final void execute() throws Throwable {
        LinkedList<TypedMessage> msgList = new LinkedList();
        while (!isStopped()) {
            try {
                tack(msgList);
            } catch (InterruptedException ex) {
                if (!isStopped()) {
                    //没有退出请求任务继续运行
                    Thread.interrupted();
                    logger.warn("wait message interrupted", ex);
                } else {
                    return;
                }
            }
            for (TypedMessage m : msgList) {
                execute(m);
            }
            msgList.clear();
        }
    }

    private final void execute(TypedMessage tmsg) throws Throwable {
        if (tmsg.type == MessageType.INFO) {
            handle_info(tmsg.message, tmsg.from);
        } else {
            if (tmsg.type == MessageType.CALL) {
                try {
                    tmsg.result.msg = handle_call(tmsg.message, tmsg.from);
                    tmsg.result.resulted = true;
                } catch (Throwable ex) {
                    //当被调用Task异常退出时，调用者也应抛出异常
                    tmsg.result.msg = new ThrowMessage(ex);
                    throw ex;
                } finally {
                    synchronized (tmsg.result) {
                        tmsg.result.notify();
                    }
                }
            }
        }
    }

    public final void putInfo(Message msg) {
        assert (msg != null);
        try {
            //此处长度判断与put不作同步，最多使队列长度达到maxQueueLen+并发线程数
            if (maxQueueLen > 0 && queue.size() >= maxQueueLen) {
                    throw new TaskQueueOverflowException("ChildTask " + getName() + "'s queue overflow");
                }
            TypedMessage tmsg = new TypedMessage(MessageType.INFO, msg, Thread.currentThread().getName());
            queue.put(tmsg);
        } catch (InterruptedException ex) {
            throw new TaskInterruptedException("send interrupted  ***  info=" + msg.name +
                    ",sender=" + Thread.currentThread().getName() +
                    ",dest=" + getName(), ex);
        }
    }

    public final void putCall(Message msg, Result result) {
        assert (msg != null && result != null);
        try {
            //此处长度判断与put不作同步，最多使队列长度达到maxQueueLen+并发线程数
            if (maxQueueLen > 0 && queue.size() >= maxQueueLen) {
                throw new TaskQueueOverflowException("ChildTask " + getName() + "'s queue overflow");
            }
            TypedMessage tmsg = new TypedMessage(MessageType.CALL, msg, Thread.currentThread().getName(), result);
            queue.put(tmsg);
        } catch (InterruptedException ex) {
            throw new TaskInterruptedException("call interrupted  ***  method=" + msg.name +
                    ",caller=" + Thread.currentThread().getName() +
                    ",dest=" + getName(), ex);
        }
    }

    protected final Message receive(IMessageMatcher matcher, long timeout) throws TimeoutException, Throwable {
        assert (matcher != null);
        //receive只应在本ChildTask线程中访问
        assert (getName().equals(Thread.currentThread().getName()));
        List<TypedMessage> temp = new LinkedList();
        TypedMessage tmsg;
        while (!isStopped()) {
            tmsg = queue.poll(timeout, TimeUnit.MILLISECONDS);
            if (tmsg != null) {
                if (tmsg.type == MessageType.INFO) {
                    Message ret = matcher.match(tmsg.message);
                    if (ret != null) {
                        synchronized (queue) {
                            queue.drainTo(temp);
                            queue.addAll(temp);
                        }
                        temp.clear();
                        return ret;
                    } else {
                        temp.add(tmsg);
                    }
                } else {
                    temp.add(tmsg);
                }
            } else {//timeout
                synchronized (queue) {
                    queue.drainTo(temp);
                    queue.addAll(temp);
                }
                temp.clear();
                throw new TimeoutException();
            }
        }
        synchronized (queue) {
            queue.drainTo(temp);
            queue.addAll(temp);
        }
        temp.clear();
        throw new InterruptedException();
    }

    protected final Message receive(IMessageMatcher matcher) throws TimeoutException, Throwable {
        assert (matcher != null);
        //receive只应在本ChildTask线程中访问
        assert (getName().equals(Thread.currentThread().getName()));
        List<TypedMessage> temp = new LinkedList();
        TypedMessage tmsg;
        while (!isStopped()) {
            tmsg = queue.take();
            if (tmsg.type == MessageType.INFO) {
                Message ret = matcher.match(tmsg.message);
                if (ret != null) {
                    synchronized (queue) {
                        queue.drainTo(temp);
                        queue.addAll(temp);
                    }
                    temp.clear();
                    return ret;
                } else {
                    temp.add(tmsg);
                }
            } else {
                temp.add(tmsg);
            }
        }
        synchronized (queue) {
            queue.drainTo(temp);
            queue.addAll(temp);
        }
        temp.clear();
        throw new InterruptedException();
    }

    @Override
    public final void run() {
        logger.trace("ChildTask '" + getName() + "' start up");
        RuntimeException runtimeEx = null;
        try {
            this.init();
            this.execute();
        } catch (RuntimeException ex) {
            runtimeEx = ex;
        } catch (Throwable ex) {
            runtimeEx = new RuntimeException(ex);
        }
        logger.trace("ChildTask '" + getName() + "' stopped");
        if (stopInfo == null) { //没有退出请求，根据是否有异常进行处理
            terminate(runtimeEx);
            if (runtimeEx == null) {
                stopInfo = "normal";
            } else {
                stopInfo = runtimeEx.getMessage();
                if (stopInfo == null) {
                    stopInfo = runtimeEx.getClass().getSimpleName();
                }
                throw runtimeEx;
            }
        } else {
            if (stopInfo.equals("normal")) { //收到正常退出请求，不抛出异常
                terminate(null);
            } else { //收到错误退出请求
                if (runtimeEx == null) {
                    runtimeEx = new RuntimeException(stopInfo);
                    terminate(runtimeEx);
                    throw runtimeEx;
                } else {
                    terminate(runtimeEx);
                    throw runtimeEx;
                }
            }
        }
    }

    public final void errorStopRequest(String error) {
        stopInfo = error;
        this.interrupt();
    }

    public final boolean isStopped() {
        return stopInfo != null;
    }

    public final boolean isErrorStopped() {
        return stopInfo != null && !stopInfo.equals("normal");
    }

    public final boolean isNormalStopped() {
        return stopInfo != null && stopInfo.equals("normal");
    }

    public final String getStopInfo() {
        return stopInfo;
    }

    @Override
    public final void normalStopRequest() {
        stopInfo = "normal";
        this.interrupt();
    }

    public long getMaxQueueLen() {
        return maxQueueLen;
    }

    protected abstract void init() throws Throwable;

    protected abstract void terminate(Throwable ex);
}
