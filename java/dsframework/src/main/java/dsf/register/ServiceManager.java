package dsf.register;

import dsf.core.ChildInfo;
import dsf.core.Message;
import dsf.core.MessageChildTask;
import dsf.core.TaskContext;
import dsf.core.ThrowMessage;
import dsf.pool.IServiceStateListener;
import dsf.route.NoUseableServiceException;
import dsf.route.RateLimitedException;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author arksea
 */
public class ServiceManager extends MessageChildTask<HashMap<String, Service>> {

    public static String TASK_NAME = "svcmanager";
    public static long DEFAULT_TIMEOUT = 10000;
    private final static Logger logger = LogManager.getLogger(ServiceManager.class.getName());
    private static final TaskContext context = TaskContext.instance();

    public static ChildInfo createChildInfo(int maxQueueSize) {
        return new ChildInfo(ServiceManager.TASK_NAME, ServiceManager.class, new HashMap<String,Service>(), maxQueueSize);
    }
    //---------------------------------------------------------------------------
    //以下系列方法为 ServiceManager 导出的API，
    //这些静态方法仅是为了简化TaskContext.call()的使用,
    //在MessageChildTask中将会频繁看到这种范式

    public static Boolean isSubscribed(String regname) {
        Message ret = context.call(TASK_NAME, new Message("is_subed", regname), DEFAULT_TIMEOUT);
        return (Boolean) ret.value;
    }

    public static Set<String> getSubscribed() {
        Message ret = context.call(TASK_NAME, new Message("get_subed", ""), DEFAULT_TIMEOUT);
        return (Set<String>) ret.value;
    }

    public static void addSubscribe(MsgSubscribeResult sub) {
        context.call(TASK_NAME, new Message("add_sub", sub), DEFAULT_TIMEOUT);
    }

    public static ServiceInstance getServiceInstance(String regname) {
        Message ret = context.call(TASK_NAME, new Message("get_instance", regname), DEFAULT_TIMEOUT);
        return (ServiceInstance) ret.value;
    }

    public static List<ServiceInstance> getUsableServiceInstances(String regname) {
        Message ret = context.call(TASK_NAME, new Message("get_usable_instances", regname), DEFAULT_TIMEOUT);
        return (List<ServiceInstance>) ret.value;
    }

    public static MsgServiceDefine getServiceDefine(String regname) {
        Message result = context.call(TASK_NAME, new Message("get_svcdef", regname), DEFAULT_TIMEOUT);
        return (MsgServiceDefine) result.value;
    }
    
    public static void registerStateListener(final IServiceStateListener l) {
        context.send(TASK_NAME, new Message("reg_state_listener", l));
    }
    
    public static void unregisterStateListener(final IServiceStateListener l) {
        context.send(TASK_NAME, new Message("unreg_state_listener", l));
    }

    //注册服务器发来的远端通知
    public static void notifyServiceState(MsgNotifySvcState notify) {
        context.send(TASK_NAME, new Message("notify_svc_state", notify));
    }

    public static void notifySvcdefUpdate(MsgServiceDefine notify) {
        context.send(TASK_NAME, new Message("notify_svcdef_update", notify));
    }

    private static class FailedArg {

        public String regname;
        public MsgSvcAddr svcAddr;
        public Throwable exception;
    }

    private static class SucceedArg {

        public String regname;
        public MsgSvcAddr svcAddr;
    }

    //---------------------------------------------------------------------------
    public ServiceManager(String name, long maxQueueLen, HashMap<String,Service> state) {
        super(name, maxQueueLen, state);
    }

    @Override
    protected void handle_info(Message msg, String from) throws Throwable {
        String key = msg.name;
        if (key.equals("notify_svc_state")) {
            MsgNotifySvcState notify = (MsgNotifySvcState) msg.value;
            Service service = state.get(notify.regname);
            if (service != null) {
                service.updateStates(notify.addr, notify.states);
            } else {
                logger.warn("notify_svc_state() not find service: " + notify.regname);
            }
        } else if (key.equals("notify_svcdef_update")) {
            MsgServiceDefine svcdef = (MsgServiceDefine) msg.value;
            Service svc = state.get(svcdef.regname);
            if (svc!=null) {
                svc.updateDefine(svcdef);
            } else {
                logger.warn("notify_svcdef_update() not find service: " + svcdef.regname);
            }
        } else if (key.equals("reg_state_listener")) {
            IServiceStateListener l = (IServiceStateListener) msg.value;
            String regname = l.getServiceRegname();
            Service service = state.get(regname);
            if (service == null) {
                logger.error("service not be subscribed: " + regname);
            } else {
                service.registerStateListener(l);
            }
        } else if (key.equals("unreg_state_listener")) {
            IServiceStateListener l = (IServiceStateListener) msg.value;
            String regname = l.getServiceRegname();
            Service service = state.get(regname);
            if (service == null) {
                logger.error("service not be subscribed: " + regname);
            } else {
                service.unregisterStateListener(l);
            }
        } else {
            logger.warn("unknow info message: " + msg);
        }
    }

    @Override
    protected Message handle_call(Message msg, String from) throws Throwable {
        String key = msg.name;
        if (key.equals("get_instance")) {
            String regname = (String) msg.value;
            Service service = state.get(regname);
            if (service == null) {
                return new ThrowMessage("service not be subscribed: " + regname);
            }
            try {
                ServiceInstance inst = service.getInstance();
                return new Message("ok", inst);
            } catch (NoUseableServiceException ex) {
                return new ThrowMessage("no usable service instance: " + regname);
            } catch (RateLimitedException ex) {
                return new ThrowMessage("service rate limited: " + regname);
            }
        } else if (key.equals("get_usable_instances")) {
            String regname = (String) msg.value;
            Service service = state.get(regname);
            if (service == null) {
                return new ThrowMessage("service not be subscribed: " + regname);
            }

            List<ServiceInstance> list = service.getInstanceList();
            List<ServiceInstance> ret = new LinkedList<ServiceInstance>();
            for (int i = 0; i < list.size(); ++i) {
                ServiceInstance s = list.get(i);
                String online = (String) s.getState("online");
                if (online != null && online.equals("true")) {
                    ret.add(s);
                }
            }
            return new Message("ok", ret);
        } else if (key.equals("get_svcdef")) {
            String regname = (String) msg.value;
            Service service = state.get(regname);
            if (service == null) {
                return new Message("ok", null);
            } else {
                MsgServiceDefine def = service.define;
                return new Message("ok", def);
            }
        } else if (key.equals("is_subed")) {
            String regname = (String) msg.value;
            Service service = state.get(regname);
            return new Message("ok", service != null);
        } else if (key.equals("add_sub")) {
            MsgSubscribeResult sub = (MsgSubscribeResult) msg.value;
            Service service = new Service(sub);
            state.put(sub.svcdef.regname, service);
            return new Message("ok", "");
        } else if (key.equals("get_subed")) {
            Set<String> subed = state.keySet();
            return new Message("ok", subed);
        }
        return new ThrowMessage("undefined call: " + key);
    }

    @Override
    protected void init() throws Throwable {
    }

    @Override
    protected void terminate(Throwable ex) {
    }
}
