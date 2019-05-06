package dsf.core;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 * @author arksea
 */
public class TaskContext {

    private static final TaskContext context = new TaskContext();
    private final Map<String, MessageChildTask> serviceMap = new ConcurrentHashMap();
    private final List<Supervisor> supList = new LinkedList();
    private final Timer timer = new Timer("TaskContext Timer", true);

    public static TaskContext instance() {
        return context;
    }

    public synchronized Supervisor start(String supervisorName, RestartStrategies restart, ChildInfo[] childs) {
        Supervisor sup = new Supervisor(supervisorName, restart, childs);
        supList.add(sup);
        return sup;
    }
    
    public synchronized Supervisor start(RestartStrategies restart, ChildInfo child) {
        Supervisor sup = new Supervisor(child.name, restart, new ChildInfo[]{child});
        supList.add(sup);
        return sup;
    }

    public synchronized void stopAll() {
        for (int i = supList.size()-1; i>=0; --i) {
            Supervisor s = supList.get(i);
            s.stopAll();
        }
        supList.clear();
    }

    public synchronized void stop(String supervisorName) {
        for (Supervisor s: supList) {
            if (s.name.equals(supervisorName)) {
                s.stopAll();
                supList.remove(s);
                break;
            }
        }
    }

    private TaskContext() {
    }

    protected void put(String name, MessageChildTask svc) {
        //不能直接替换子任务，否则被替换者将会成为一个“悬挂”的同名子任务
        if (serviceMap.get(name) != null) {
            throw new RuntimeException("register a ChildTask failed,The name '" + name + "' is exist");
        }
        serviceMap.put(name, svc);
    }

    protected void remove(String name) {
        serviceMap.remove(name);
    }

    protected MessageChildTask get(String name) {
        return serviceMap.get(name);
    }

    public boolean exist(String name) {
        return serviceMap.containsKey(name);
    }

    //发送消息给目标任务，这种调用不会阻塞调用者，也不会阻塞被调用者
    public void send(String dest, Message msg) {
        MessageChildTask svc = get(dest);
        if (svc != null) {
            svc.putInfo(msg);
        } else {
            throw new TaskNotExistException(dest);
        }
    }
// 直接向Task引用发送消息，会在Task被重启后发给一个已经死掉的Task
//    public void send(MessageChildTask svc, Message msg) {
//        if (svc != null) {
//            svc.putInfo(msg);
//        }
//    }
    //延迟发送消息给目标任务，这种调用不会阻塞调用者，也不会阻塞被调用者，
    //注意：对于向自身投递消息的Task，如果延时期间失败被重启，消息不会被送到新的任务线程中
    public void send_after(final long milliseconds, final String dest, final Message msg) {
        final MessageChildTask svc = get(dest);
        final String source = Thread.currentThread().getName();
        if (svc != null) {
            TimerTask task = new TimerTask() {

                @Override
                public void run() {
                    if (source.equals(dest)) {
                        //向自己投递延时消息将直接使用消息发送时取得的svc实例发送消息，
                        //这样，如果延时期间失败被重启，消息不会被送到新的任务线程中
                        //对比重新根据任务名取得新的实例应用svcnew = get(dest)，这种选择副作用较小,
                        svc.putInfo(msg);
                    } else {
                        //对于外部线程发送的消息，将尽量投递到新的有效的任务线程中
                        MessageChildTask newsvc = get(dest);
                        if (newsvc != null) {
                            newsvc.putInfo(msg);
                        }
                    }

                }
            };
            timer.schedule(task, milliseconds);
        } else {
            throw new TaskNotExistException(dest);
        }
    }
//    public void send_after(final long milliseconds, final MessageChildTask svc, final Message msg) {
//        assert (svc!=null);
//        TimerTask task = new TimerTask() {
//            @Override
//            public void run() {
//                svc.putInfo(msg);
//            }
//        };
//        timer.schedule(task, milliseconds);
//    }
    //发送消息给目标任务，要求其处理请求后返回处理结果，这种调用会阻塞调用者，也会阻塞被调用者
    public Message call(String dest, Message callmsg, long timeout) {
        final MessageChildTask svc = get(dest);
        if (svc == null) {
            throw new TaskNotExistException("task not exist  ***  method=" + callmsg.name +
                    ",caller=" + Thread.currentThread().getName() +
                    ",dest=" + dest);
        }
        return call(svc, callmsg, timeout);
    }
    private Message call(MessageChildTask svc, Message callmsg, long timeout) {
        //不得从任务线程本身call自己，防止死锁
        assert (svc!=null && !svc.getName().equals(Thread.currentThread().getName()));
        String dest = svc.getName();
        if (timeout == 0) {
            throw new RuntimeException("timeout must not be zero.");
        }
        //每次都创建新的Result实例，并在其上wait/notify，防止错误的信号通知
        Result result = new Result();
        synchronized (result) {
            svc.putCall(callmsg, result);
            try {
                result.wait(timeout);
            } catch (InterruptedException ex) {
                throw new TaskInterruptedException("call interrupted  ***  method=" + callmsg.name +
                        ",caller=" + Thread.currentThread().getName() +
                        ",dest=" + dest, ex);
            }
        }
        if (result.msg != null && (result.msg instanceof ThrowMessage)) {
            Object v = result.msg.value;
            if (v instanceof Throwable) {
                throw new TaskCallException("  ***  method=" + callmsg.name +
                        ",caller=" + Thread.currentThread().getName() +
                        ",dest=" + dest, (Throwable) v);
            } else {
                throw new TaskCallException((String) v + "  ***  method=" + callmsg.name +
                        ",caller=" + Thread.currentThread().getName() +
                        ",dest=" + dest);
            }
        } else {
            if (!result.resulted) {
                throw new TaskCallException("call timeout  ***  method=" + callmsg.name +
                        ",caller=" + Thread.currentThread().getName() +
                        ",dest=" + dest);
            } else {
                return result.msg;
            }
        }
    }
}
