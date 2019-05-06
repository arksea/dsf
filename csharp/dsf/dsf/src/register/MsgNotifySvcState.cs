using System.Collections.Generic;

namespace com.baidu.dsf.register
{

    /// <summary>
    /// 服务状态通知消息
    /// {
    ///  "regname":"service1 ver1.0",                  //服务注册名,通常包含路径、服务名与版本信息
    ///  "addr":{"host":"192.168.254.81","port":9090}, //服务实例地址信息
    ///  "states":[{"name":"online", "value":"true"}]  //服务状态列表
    /// }
    /// @author arksea
    /// </summary>
    public class MsgNotifySvcState
    {

        public readonly string regname;
        public readonly MsgSvcAddr addr;
        public readonly IList<MsgSvcState> states;

        public MsgNotifySvcState(string regname, MsgSvcAddr addr, IList<MsgSvcState> states)
        {
            this.regname = regname;
            this.addr = addr;
            this.states = states;
        }
    }

}