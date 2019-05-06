using System;
using System.Collections.Generic;

namespace com.baidu.dsf.register
{

    using com.baidu.dsf.core;
    using com.baidu.dsf.route;
    using java.util;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class ServiceManager : MessageChildTask //<Dictionary<string, Service>>
    {

        public static string TASK_NAME = "svcmanager";
        public static long DEFAULT_TIMEOUT = 10000;
        private static readonly TaskContext context = TaskContext.instance();

        public static ChildInfo createChildInfo(int maxQueueSize)
        {
            return new ChildInfo(ServiceManager.TASK_NAME, typeof(ServiceManager), new HashMap(), maxQueueSize);
        }
        //---------------------------------------------------------------------------
        //以下系列方法为 ServiceManager 导出的API，
        //这些静态方法仅是为了简化TaskContext.call()的使用,
        //在MessageChildTask中将会频繁看到这种范式

        public static bool isSubscribed(string regname)
        {
            Message ret = context.call(TASK_NAME, new Message("is_subed", regname), DEFAULT_TIMEOUT);
            return (bool) ret.value;
        }

        public static HashSet<string> Subscribed
        {
            get
            {
                Message ret = context.call(TASK_NAME, new Message("get_subed", ""), DEFAULT_TIMEOUT);
                return (HashSet<string>) ret.value;
            }
        }

        public static void addSubscribe(MsgSubscribeResult sub)
        {
            context.call(TASK_NAME, new Message("add_sub", sub), DEFAULT_TIMEOUT);
        }

        public static ServiceInstance getServiceInstance(string regname)
        {
            Message ret = context.call(TASK_NAME, new Message("get_instance", regname), DEFAULT_TIMEOUT);
            return (ServiceInstance) ret.value;
        }

        public static IList<ServiceInstance> getUsableServiceInstances(string regname)
        {
            Message ret = context.call(TASK_NAME, new Message("get_usable_instances", regname), DEFAULT_TIMEOUT);
            return (IList<ServiceInstance>) ret.value;
        }

        public static MsgServiceDefine getServiceDefine(string regname)
        {
            Message result = context.call(TASK_NAME, new Message("get_svcdef", regname), DEFAULT_TIMEOUT);
            return (MsgServiceDefine) result.value;
        }

        //注册服务器发来的远端通知
        public static void notifyServiceState(MsgNotifySvcState notify)
        {
            context.send(TASK_NAME, new Message("notify_svc_state", notify));
        }

        public static void notifySvcdefUpdate(MsgServiceDefine notify)
        {
            context.send(TASK_NAME, new Message("notify_svcdef_update", notify));
        }

        //---------------------------------------------------------------------------
        public ServiceManager(string name, int maxQueueLen, HashMap state) : base(name, maxQueueLen, state)
        {
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected void handle_info(com.baidu.dsf.core.Message msg, String from) throws Throwable
        protected override internal void handle_info(Message msg, string from)
        {
            string key = msg.name;
            HashMap s = (HashMap)state;
            if (key.Equals("notify_svc_state"))
            {
                MsgNotifySvcState notify = (MsgNotifySvcState) msg.value;
                Service service = (Service)s.get(notify.regname);
                if (service != null)
                {
                    service.updateStates(notify.addr, notify.states);
                }
                else
                {
                    DSFLogHelper.ErrorLog("ServiceManager", "notify_svc_state() not find service: " + notify.regname);
                }
            }
            else if (key.Equals("notify_svcdef_update"))
            {
                MsgServiceDefine svcdef = (MsgServiceDefine) msg.value;
                Service svc = (Service)s.get(svcdef.regname);
                if (svc != null)
                {
                    svc.updateDefine(svcdef);
                }
                else
                {
                    DSFLogHelper.ErrorLog("ServiceManager", "notify_svcdef_update() not find service: " + svcdef.regname);
                }
            }
            else
            {
                DSFLogHelper.ErrorLog("ServiceManager", "unknow info message: " + msg);
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected com.baidu.dsf.core.Message handle_call(com.baidu.dsf.core.Message msg, String from) throws Throwable
        protected override internal Message handle_call(Message msg, string from)
        {
            string key = msg.name;
            HashMap s = (HashMap)state;
            if (key.Equals("get_instance"))
            {
                string regname = (string) msg.value;
                Service service = (Service)s.get(regname);
                if (service == null)
                {
                    return new ThrowMessage("service not be subscribed: " + regname);
                }
                try
                {
                    ServiceInstance inst = service.Instance;
                    return new Message("ok", inst);
                }
                catch (NoUseableServiceException)
                {
                    return new ThrowMessage("no usable service instance: " + regname);
                }
                catch (RateLimitedException)
                {
                    return new ThrowMessage("service rate limited: " + regname);
                }
            }
            else if (key.Equals("get_usable_instances"))
            {
                string regname = (string) msg.value;
                Service service = (Service)s.get(regname);
                if (service == null)
                {
                    return new ThrowMessage("service not be subscribed: " + regname);
                }

                java.util.List list = service.instanceList;
                IList<ServiceInstance> ret = new List<ServiceInstance>();
                java.util.Iterator it = list.iterator();
                while(it.hasNext())
                for (int i = 0; i < list.size(); ++i)
                {
                    ServiceInstance inst = (ServiceInstance)it.next();
                    string online = (string) inst.getState("online");
                    if (online != null && online.Equals("true"))
                    {
                        ret.Add(inst);
                    }
                }
                return new Message("ok", ret);
            }
            else if (key.Equals("get_svcdef"))
            {
                string regname = (string) msg.value;
                Service service = (Service)s.get(regname);
                if (service == null)
                {
                    return new Message("ok", null);
                }
                else
                {
                    MsgServiceDefine def = service.define;
                    return new Message("ok", def);
                }
            }
            else if (key.Equals("is_subed"))
            {
                string regname = (string) msg.value;
                Service service = (Service)s.get(regname);
                return new Message("ok", service != null);
            }
            else if (key.Equals("add_sub"))
            {
                MsgSubscribeResult sub = (MsgSubscribeResult) msg.value;
                Service service = new Service(sub);
                s.put(sub.svcdef.regname, service);
                return new Message("ok", "");
            }
            else if (key.Equals("get_subed"))
            {
                Set values = s.keySet();
                HashSet<string> subed = new HashSet<string>();
                Iterator it = values.iterator();
                while(it.hasNext())
                {
                    subed.Add((string)it.next());
                }
                return new Message("ok", subed);
            }
            return new ThrowMessage("undefined call: " + key);
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected void init() throws Throwable
        protected override internal void init()
        {
        }

        protected override internal void terminate(Exception ex)
        {
        }
    }

}