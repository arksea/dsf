namespace com.baidu.dsf.register
{

    /// <summary>
    /// 服务状态报告消息
    /// {
    ///  "svc":              //服务注册消息，参考MsgServiceReg
    ///      {"regname":"service1", "host":"192.168.254.81", "port":9090},
    ///  "name":"online",    //状态名
    ///  "value":"true"      //状态值
    /// }
    /// @author arksea
    /// </summary>
    public class MsgReportSvcState
    {

        public readonly MsgServiceReg svc;
        public readonly string name;
        public readonly object value;

        public MsgReportSvcState(MsgServiceReg svc, string stateName, object stateValue)
        {
            this.svc = svc;
            this.name = stateName;
            this.value = stateValue;
        }
    }

}