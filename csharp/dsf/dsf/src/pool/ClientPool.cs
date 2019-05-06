namespace com.baidu.dsf.pool
{
    using org.apache.commons.pool2;
    using org.apache.commons.pool2.impl;
    using Thrift;

    /// <summary>
    /// 连接池
    /// @author arksea </summary>
    /// @param <T> thrift client </param>
    public class ClientPool<I> : GenericObjectPool<TServiceClient>, System.IDisposable
    {

        public ClientPool(ClientFactory<I> factory, GenericObjectPoolConfig cfg) : base(factory, cfg)
        {
        }

        public void Dispose()
        {
            if (!this.Closed)
            {
                this.close();
            }
        }

        public virtual string ServiceRegname
        {
            get
            {
                ClientFactory<I> f = (ClientFactory<I>) Factory;
                return f.define.regname;
            }
        }
    }

}