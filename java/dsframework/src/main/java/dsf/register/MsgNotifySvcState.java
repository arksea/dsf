package dsf.register;

import java.util.List;

/**
 * 服务状态通知消息
 * {
 *  "regname":"service1 ver1.0",                  //服务注册名,通常包含路径、服务名与版本信息
 *  "addr":{"host":"192.168.254.81","port":9090}, //服务实例地址信息
 *  "states":[{"name":"online", "value":"true"}]  //服务状态列表
 * }
 * @author arksea
 */
public class MsgNotifySvcState {

    public final String regname;
    public final MsgSvcAddr addr;
    public final List<MsgSvcState> states;

    public MsgNotifySvcState(String regname, MsgSvcAddr addr, List<MsgSvcState> states) {
        this.regname = regname;
        this.addr = addr;
        this.states = states;
    }
}
