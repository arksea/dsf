using System;

namespace com.baidu.dsf.register
{

    /// <summary>
    /// 服务实例地址消息
    /// {
    ///  "host": "192.168.254.81",      //服务实例主机名
    ///  "port": 9090                   //服务实例端口号
    /// }
    /// @author arksea
    /// </summary>
    public class MsgSvcAddr : IComparable
    {

        public readonly string host;
        public readonly int port;

        public MsgSvcAddr(string host, int port)
        {
            this.host = host;
            this.port = port;
        }

        public override bool Equals(object obj)
        {
            if (this == obj)
            {
                return true;
            }
            if (obj is MsgSvcAddr)
            {
                MsgSvcAddr another = (MsgSvcAddr) obj;
                return host.Equals(another.host) && port == another.port;
            }
            return false;
        }

        public override int GetHashCode()
        {
            return host.GetHashCode() + port;
        }

        public override string ToString()
        {
            return host + ":" + port;
        }

        public virtual int CompareTo(object o)
        {
            return this.ToString().CompareTo(o.ToString());
        }
    }


}