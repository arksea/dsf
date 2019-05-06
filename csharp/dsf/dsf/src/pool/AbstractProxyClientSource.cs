using System.Collections.Generic;

namespace com.baidu.dsf.pool
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public abstract class AbstractProxyClientSource<I> : IClientSource<I>
    {
        public abstract RawClientInfo getRowClient();
        public abstract IList<RawClientInfo> getRowClients();
        public abstract void returnRowClient(RawClientInfo p); //归还一个客户端
        public abstract I getClient();
        public abstract void returnClient(object p); //归还一个客户端
        public abstract string getServiceRegname();
    }

}