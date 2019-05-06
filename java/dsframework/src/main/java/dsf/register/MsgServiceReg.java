package dsf.register;

/**
 * 服务实例注册消息
 * {
 *  "regname":"service1 ver1.0",   //服务注册名,通常包含路径、服务名与版本信息
 *  "host": "192.168.254.81",      //服务实例主机名
 *  "port": 9090                   //服务实例端口号
 * }
 * @author arksea
 */
public class MsgServiceReg {

    public final String regname;
    public final String host;
    public final int port;

    public MsgServiceReg(String regname, String host, int port) {
        this.regname = regname;
        this.host = host;
        this.port = port;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof MsgServiceReg) {
            MsgServiceReg another = (MsgServiceReg) obj;
            return regname.equals(another.regname) && host.equals(another.host) && port == another.port;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return regname.hashCode() * 37 + host.hashCode() + port;
    }
}
