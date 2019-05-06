package dsf.register;

/**
 * 服务实例地址消息
 * {
 *  "host": "192.168.254.81",      //服务实例主机名
 *  "port": 9090                   //服务实例端口号
 * }
 * @author arksea
 */
public class MsgSvcAddr implements Comparable {

    public final String host;
    public final int port;

    public MsgSvcAddr(String host, int port) {
        this.host = host;
        this.port = port;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof MsgSvcAddr) {
            MsgSvcAddr another = (MsgSvcAddr) obj;
            return host.equals(another.host) && port == another.port;
        }
        return false;
    }

    @Override
    public int hashCode() {
        return host.hashCode() + port;
    }

    @Override
    public String toString() {
        return host + ":" + port;
    }

    @Override
    public int compareTo(Object o) {
        return this.toString().compareTo(o.toString());
    }
}

