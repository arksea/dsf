package dsf.register;

import static dsf.register.MSG_NAMES.*;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import dsf.DSFrameworkException;
import dsf.core.ChildInfo;
import dsf.core.IMessageMatcher;
import dsf.core.Message;
import dsf.core.MessageChildTask;
import dsf.core.NamedMsgsMatcher;
import dsf.core.TaskContext;
import dsf.core.ThrowMessage;
import java.io.IOException;
import java.util.List;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.Comparator;
import java.util.LinkedList;

/**
 *
 * @author arksea
 */
public class RegisterClient extends MessageChildTask<RegisterClient.TaskState> {

    public static String TASK_NAME = "regclient";
    public static String LOGIN_NAME = "anonymous";
    public static long DEFAULT_TIMEOUT = 10000;
    private final static Logger logger = LogManager.getLogger(RegisterClient.class.getName());
    private int missAckTickCount = 0;
    private long tickid = 0;
    private ISocketWritter writter;

    private AsynchronousSocketChannel channel;
    private CompletionReader reader;
//兼容JDK6的代码，JDK6中没有AsynchronousSocketChannel类
//    private Socket channel;
//    private SocketReaderSync reader;

    private static TaskContext context = TaskContext.instance();
    private final IMessageMatcher succeedMatcher = new NamedMsgsMatcher(MSG_TCP, new String[]{MSG_SUCCEED, MSG_ERROR_RESULT});

    public static ChildInfo createChildInfo(String loginName, InetSocketAddress[] addrs, int maxQueueSize) {
        RegisterClient.LOGIN_NAME = loginName;
        TaskState s = new TaskState(addrs);
        return new ChildInfo(RegisterClient.TASK_NAME, RegisterClient.class, s, maxQueueSize);
    }
    
    public static class TaskState {

        private final InetSocketAddress[] addrs;
        private String subid = "0";
        private long retryDelay = 0; // 重连延迟时间,首次连接不延迟
        private int sockIdx = 0;

        public TaskState(InetSocketAddress[] addrs) {
            //对地址进行排序，让各台注册服务器组成主备关系
            List<InetSocketAddress> addrList = new LinkedList<>();
            for (InetSocketAddress addr : addrs) {
                addrList.add(addr);
            }
            addrList.sort(new Comparator<InetSocketAddress>() {
                @Override
                public int compare(InetSocketAddress o1, InetSocketAddress o2) {
                    String str1 = o1.toString();
                    String str2 = o2.toString();
                    return str1.compareTo(str2);
                }
            });
            this.addrs = addrList.toArray(new InetSocketAddress[addrs.length]);
        }

        public synchronized void delay() throws InterruptedException {
            if (retryDelay > 0) {
                sleep(retryDelay);
                retryDelay *= 2;
                if (retryDelay > 30000) {
                    retryDelay = 30000;
                }
            } else { // 首次连接不延迟
                retryDelay = 2000;
            }
        }

        public synchronized void resetDelay() {
            retryDelay = 2000;
        }
    }

    // ---------------------------------------------------------------------------
    // 以下系列方法为 RegisterClient 导出的API，
    // 这些静态方法仅是为了简化TaskContext.call()的使用,
    // 在MessageChildTask中将会频繁看到这种范式
    public static void subscribeService(String regname) {
        context.call(TASK_NAME, new Message(MSG_SUBSCRIBE, regname), DEFAULT_TIMEOUT);
    }

    public static void registerService(MsgServiceReg svc) {
        context.call(TASK_NAME, new Message(MSG_REGSVC, svc), DEFAULT_TIMEOUT);
    }

    public static void unregisterService(MsgServiceReg svc) {
        context.call(TASK_NAME, new Message(MSG_UNREGSVC, svc), DEFAULT_TIMEOUT);
    }

    public static void reportServiceState(MsgServiceReg svc, String name, String value) {
        MsgReportSvcState obj = new MsgReportSvcState(svc, name, value);
        context.send(TASK_NAME, new Message(MSG_REPORT_SVC_STATE, obj));
    }
    
    public static void setServiceState(MsgServiceReg svc, List<MsgSvcState> states) {
        context.call(TASK_NAME, new Message(MSG_SET_SVC_STATE, new MsgNotifySvcState(svc.regname,new MsgSvcAddr(svc.host,svc.port),states)), DEFAULT_TIMEOUT);
    }

    public static MsgServiceDefine queryServiceDefine(String regname) {
        Message ret = context.call(TASK_NAME, new Message(MSG_QUERY_SVCDEF, regname), DEFAULT_TIMEOUT);
        MsgServiceDefine def = (MsgServiceDefine) ret.value;
        return def;
    }

    public static MsgSubscribeResult queryServiceState(String regname) {
        Message ret = context.call(TASK_NAME, new Message(MSG_QUERY_SVC_STATE, regname), DEFAULT_TIMEOUT);
        MsgSubscribeResult sub = (MsgSubscribeResult) ret.value;
        return sub;
    }

    private static class SvcdefIDL {
        MsgServiceDefine def;
        String idl;
    }
    public static void addServiceDefine(MsgServiceDefine def, String idl) {
        SvcdefIDL v = new SvcdefIDL();
        v.def = def;
        v.idl = idl;
        Message param = new Message(MSG_ADD_SVCDEF, v);
        context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
    }

    public static void updateServiceDefine(MsgServiceDefine def) {
        Message param = new Message(MSG_UPDATE_SVCDEF, def);
        context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
    }

    public static void updateServiceDefine(MsgServiceDefine def, String idl) {
        SvcdefIDL v = new SvcdefIDL();
        v.def = def;
        v.idl = idl;
        Message param = new Message(MSG_UPDATE_SVCDEF_IDL, v);
        context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
    }

    public static void deleteServiceDefine(String regname) {
        Message param = new Message(MSG_DEL_SVCDEF, regname);
        context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
    }

    // ---------------------------------------------------------------------------
    public RegisterClient(String name, long maxQueueLen, TaskState state) {
        super(name, maxQueueLen, state);
    }

    @Override
    protected void init() throws Throwable {
        context = TaskContext.instance();
        state.delay();
        initSocketChannel();
        login();
        context.send_after(1000, getName(), new Message(MSG_TICK, ""));
        // 正常运行1分钟后重置重试时间，
        // 不直接调用resetDelay是为了防止登录后短时间内出现异常造成的频繁重连
        context.send_after(60000, getName(), new Message("reset_delay", ""));
    }

    @Override
    protected void terminate(Throwable ex) {
        if (channel != null) {
            try {
                channel.close();
            } catch (IOException ex1) {
                logger.warn("close socket channel error", ex1);
            }
        }
    }

    @Override
    protected void handle_info(Message msg, String from) throws Throwable {
        String key = msg.name;
        if (key.equals(MSG_TICK)) {
            if (++missAckTickCount > 3) {
                errorStopRequest("miss tick ack count > 3");
            } else {
                writter.send(MSG_TICK, tickid);
                tickid++;
                context.send_after(30000, this.getName(), msg);
            }
        } else if (key.equals(MSG_TCP)) {
            Message tcpmsg = (Message) msg.value;
            if (tcpmsg.name.equals(MSG_TACK)) {
                missAckTickCount = 0;
            } else {
                handle_tcp_info(tcpmsg.name, tcpmsg.value, from);
            }
        } else if (key.equals(MSG_REPORT_SVC_STATE)) {
            writter.send(key, msg.value);
        } else if (key.equals("channel_error")) {
            if (!isStopped()) {
                errorStopRequest((String) msg.value);
            }
        } else if (key.equals("reset_delay")) {
            state.resetDelay();
        } else {
            logger.error("unknown message : " + msg);
        }
    }

    @Override
    protected Message handle_call(Message msg, String from) throws Throwable {
        String key = msg.name;
        if (key.equals(MSG_SUBSCRIBE)) { // 订阅服务
            String regname = (String) msg.value;
            if (ServiceManager.isSubscribed(regname)) {
                return new Message("ok", "");
            }
            writter.send(msg);
            IMessageMatcher m = new NamedMsgsMatcher(MSG_TCP, new String[]{MSG_SUBSCRIBE_RESULT, MSG_ERROR_RESULT});
            Message ret = this.receive(m, DEFAULT_TIMEOUT);
            if (ret.name.equals(MSG_SUBSCRIBE_RESULT)) {
                MsgSubscribeResult sub = (MsgSubscribeResult) ret.value;
                ServiceManager.addSubscribe(sub);
                logger.info("服务订阅成功：{}", regname);
                return new Message("ok", "");
            } else {
                logger.error("服务订阅失败：{}, {}", regname, ret.value.toString());
                return new ThrowMessage(ret.value.toString());
            }
        } else if (key.equals(MSG_QUERY_SVC_STATE)) {
            writter.send(msg);
            IMessageMatcher m = new NamedMsgsMatcher(MSG_TCP, new String[]{MSG_QUERY_SVC_STATE_RESULT, MSG_ERROR_RESULT});
            Message ret = this.receive(m, DEFAULT_TIMEOUT);
            if (ret.name.equals(MSG_QUERY_SVC_STATE_RESULT)) {
                MsgSubscribeResult sub = (MsgSubscribeResult) ret.value;
                return new Message("ok", sub);
            } else {
                return new ThrowMessage(ret.value.toString());
            }
        } else if (key.equals(MSG_REGSVC)) { // 注册服务
            writter.send(key, msg.value);
            Message ret = waitForSucceedMsg();
            return ret;
        } else if (key.equals(MSG_UNREGSVC)) {
            writter.send(key, msg.value);
            Message ret = waitForSucceedMsg();
            return ret;
        } else if (key.equals(MSG_SET_SVC_STATE)) {
            writter.send(key, msg.value);
            Message ret = waitForSucceedMsg();
            return ret;
        } else if (key.equals(MSG_QUERY_SVCDEF)) { // 查询服务定义
            writter.send(msg);
            IMessageMatcher m = new NamedMsgsMatcher(MSG_TCP, new String[]{MSG_QUERY_SVCDEF_RESULT, MSG_ERROR_RESULT});
            Message ret = this.receive(m, DEFAULT_TIMEOUT);
            if (ret.name.equals(MSG_QUERY_SVCDEF_RESULT)) {
                MsgServiceDefine def = (MsgServiceDefine) ret.value;
                return new Message("ok", def);
            } else {
                return new ThrowMessage(ret.value.toString());
            }
        } else if (key.equals(MSG_ADD_SVCDEF)) { // 添加服务定义
            SvcdefIDL v = (SvcdefIDL)msg.value;
            Object[] args = new Object[] {v.def, v.idl};
            writter.send(key, args);
            return waitForSucceedMsg();
        } else if (key.equals(MSG_UPDATE_SVCDEF)) { // 修改服务定义
            writter.send(key, msg.value);
            return waitForSucceedMsg();
        } else if (key.equals(MSG_UPDATE_SVCDEF_IDL)) { // 修改服务定义
            SvcdefIDL v = (SvcdefIDL)msg.value;
            Object[] args = new Object[] {v.def, v.idl};
            writter.send(key, args);
            return waitForSucceedMsg();
        } else if (key.equals(MSG_DEL_SVCDEF)) { // 删除服务定义
            writter.send(key, msg.value);
            return waitForSucceedMsg();
        } else {
            return null;
        }
    }

    private void handle_tcp_info(String name, Object obj, String from) {
        if (name.equals(MSG_NOTIFY_SVC_STATE)) {
            MsgNotifySvcState msg = (MsgNotifySvcState) obj;
            ServiceManager.notifyServiceState(msg);
        } else if (name.equals(MSG_NOTIFY_SVCDEF_UPDATE)) {
            MsgServiceDefine msg = (MsgServiceDefine) obj;
            ServiceManager.notifySvcdefUpdate(msg);
        } else if (name.equals(MSG_ERROR)) {
            logger.error(MSG_ERROR+": "+obj);
        } else {
            logger.warn("not handle tcp info: " + name);
        }
    }

    // 等待succeed消息，所有返回succeed消息的tcp call都可以使用
    private Message waitForSucceedMsg() throws Throwable {
        Message ret = this.receive(succeedMatcher, DEFAULT_TIMEOUT);
        if (ret.name.equals(MSG_SUCCEED)) {
            String s = (String) ret.value;
            return new Message("succeed", s);
        } else {
            return new ThrowMessage(ret.value.toString());
        }
    }

    /**
     * 获取注册服务器地址
     *
     * @param addrArray
     * @return
     */
    private InetSocketAddress getSockAddr(InetSocketAddress[] addrArray) {
        if (state.sockIdx >= addrArray.length) {
            state.sockIdx = 0;
        }
        InetSocketAddress sockAddr = addrArray[state.sockIdx];
        state.sockIdx++;
        return sockAddr;
    }
    
    private void initSocketChannel() {
        InetSocketAddress addr = getSockAddr(state.addrs);
        try {
            // 轮流使用其中一个地址连接注册服务器
            channel = AsynchronousSocketChannel.open();
            channel.connect(addr).get();
            reader = new CompletionReader(channel, this);
            writter = new SocketWritter(channel);
//兼容JDK6的代码，JDK6中没有AsynchronousSocketChannel类
//            channel = new Socket();
//            channel.connect(addr);
//            reader = new SocketReaderSync(channel, this);
//            writter = new SocketWritterSync(channel);
        } catch (Throwable ex) {
            throw new RuntimeException("连接注册服务器失败," + addr.getHostName() + ":" + addr.getPort(), ex);
        }
    }

    private void login() throws Throwable {
        // 登录注册服务器
        Map<String, String> json = new HashMap<String, String>(3);
        json.put("subid", state.subid);
        json.put("name", RegisterClient.LOGIN_NAME);
        json.put("version", "1.1");

        writter.send(MSG_LOGIN, json);
        IMessageMatcher m = new NamedMsgsMatcher(MSG_TCP, new String[]{MSG_LOGIN_SUCCEED, MSG_SYNC_SUBLIST, MSG_ERROR_RESULT});
        while (true) {
            Message ret = this.receive(m, DEFAULT_TIMEOUT);
            if (ret.name.equals(MSG_LOGIN_SUCCEED)) {
                String newid = ret.value.toString();
                state.subid = newid;
                break;
            } else if (ret.name.equals(MSG_SYNC_SUBLIST)) {
                Set<String> subed = ServiceManager.getSubscribed();
                writter.send(MSG_UPDATE_SUBLIST, subed);
            } else {
                throw new DSFrameworkException(ret.toString());
            }
        }
    }
}
