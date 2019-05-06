namespace com.baidu.dsf.register
{

    /// <summary>
    /// 服务实例注册消息
    /// {
    ///  "regname":"service1 ver1.0",   //服务注册名,通常包含路径、服务名与版本信息
    ///  "host": "192.168.254.81",      //服务实例主机名
    ///  "port": 9090                   //服务实例端口号
    /// }
    /// @author arksea
    /// </summary>
    public class MsgServiceReg
    {

        public readonly string regname;
        public readonly string host;
        public readonly int port;

        public MsgServiceReg(string regname, string host, int port)
        {
            this.regname = regname;
            this.host = host;
            this.port = port;
        }

        public override bool Equals(object obj)
        {
            if (this == obj)
            {
                return true;
            }
            if (obj is MsgServiceReg)
            {
                MsgServiceReg another = (MsgServiceReg) obj;
                return regname.Equals(another.regname) && host.Equals(another.host) && port == another.port;
            }
            return false;
        }

        public override int GetHashCode()
        {
            return regname.GetHashCode() * 37 + host.GetHashCode() + port;
        }
    }

}