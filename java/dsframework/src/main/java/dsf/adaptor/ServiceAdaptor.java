package dsf.adaptor;

import dsf.pool.IServiceStateListener;
import dsf.register.MsgServiceDefine;
import dsf.register.MsgServiceReg;
import dsf.register.MsgSubscribeResult;
import dsf.register.RegisterClient;
import dsf.register.ServiceManager;

/**
 *
 * @author arksea
 */
public class ServiceAdaptor {

    public static void registerService(MsgServiceReg svc) {
        RegisterClient.registerService(svc);
    }

    public static void unregisterService(MsgServiceReg svc) {
        RegisterClient.unregisterService(svc);
    }

    public static void reportServiceState(MsgServiceReg svc, String name, String value) {
        RegisterClient.reportServiceState(svc, name, value);
    }

    public static void subscribeService(String regname) {
        if (!ServiceManager.isSubscribed(regname)) {
            RegisterClient.subscribeService(regname);
        }
    }
    
    public static MsgServiceDefine queryServiceDefine(String regname) {
        MsgServiceDefine def = ServiceManager.getServiceDefine(regname);
        if (def == null) {
            def = RegisterClient.queryServiceDefine(regname);
        }
        return def;
    }

    public static MsgSubscribeResult queryServiceState(String regname) {
        return RegisterClient.queryServiceState(regname);
    }

    /**
     * 添加服务定义
     * @param def
     */
    public static void addServiceDefine(MsgServiceDefine def, String idl) {
        RegisterClient.addServiceDefine(def, idl);
    }

    public static void updateServiceDefine(MsgServiceDefine def) {
        RegisterClient.updateServiceDefine(def);
    }

    public static void updateServiceDefine(MsgServiceDefine def, String idl) {
        RegisterClient.updateServiceDefine(def, idl);
    }

    /**
     * 删除服务定义
     * @param regname
     */
    public static void deleteServiceDefine(String regname) {
        RegisterClient.deleteServiceDefine(regname);
    }
}
