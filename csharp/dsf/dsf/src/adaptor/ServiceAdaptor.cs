namespace com.baidu.dsf.adaptor
{

    using com.baidu.dsf.register;
    using MsgServiceReg = com.baidu.dsf.register.MsgServiceReg;
    using MsgSubscribeResult = com.baidu.dsf.register.MsgSubscribeResult;
    using RegisterClient = com.baidu.dsf.register.RegisterClient;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class ServiceAdaptor
    {

        public static void registerService(MsgServiceReg svc)
        {
            RegisterClient.registerService(svc);
        }

        public static void unregisterService(MsgServiceReg svc)
        {
            RegisterClient.unregisterService(svc);
        }

        public static void reportServiceState(MsgServiceReg svc, string name, string value)
        {
            RegisterClient.reportServiceState(svc, name, value);
        }

        public static void subscribeService(string regname)
        {
            if (!ServiceManager.isSubscribed(regname))
            {
                RegisterClient.subscribeService(regname);
            }
        }

        public static MsgServiceDefine queryServiceDefine(string regname)
        {
            MsgServiceDefine def = ServiceManager.getServiceDefine(regname);
            if (def == null)
            {
                def = RegisterClient.queryServiceDefine(regname);
            }
            return def;
        }

        public static MsgSubscribeResult queryServiceState(string regname)
        {
            return RegisterClient.queryServiceState(regname);
        }

        /// <summary>
        /// 添加服务定义 </summary>
        /// <param name="def"> </param>
        public static void addServiceDefine(MsgServiceDefine def, string idl)
        {
            RegisterClient.addServiceDefine(def, idl);
        }

        public static void updateServiceDefine(MsgServiceDefine def)
        {
            RegisterClient.updateServiceDefine(def);
        }

        public static void updateServiceDefine(MsgServiceDefine def, string idl)
        {
            RegisterClient.updateServiceDefine(def, idl);
        }

        /// <summary>
        /// 删除服务定义 </summary>
        /// <param name="regname"> </param>
        public static void deleteServiceDefine(string regname)
        {
            RegisterClient.deleteServiceDefine(regname);
        }
    }

}