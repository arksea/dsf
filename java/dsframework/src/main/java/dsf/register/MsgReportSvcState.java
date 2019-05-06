package dsf.register;

/**
 * 服务状态报告消息
 * {
 *  "svc":              //服务注册消息，参考MsgServiceReg
 *      {"regname":"service1", "host":"192.168.254.81", "port":9090},
 *  "name":"online",    //状态名
 *  "value":"true"      //状态值
 * }
 * @author arksea
 */
public class MsgReportSvcState {

    public final MsgServiceReg svc;
    public final String name;
    public final Object value;

    public MsgReportSvcState(MsgServiceReg svc, String stateName, Object stateValue) {
        this.svc = svc;
        this.name = stateName;
        this.value = stateValue;
    }
}
