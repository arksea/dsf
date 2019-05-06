namespace com.baidu.dsf.pool
{

    //using com.baidu.dsf.adaptor;
    using com.baidu.dsf.register;
    using com.baidu.dsf.adaptor;
    using org.apache.commons.pool2.impl;
    using System.Collections.Generic;

    /// 
    /// <summary>
    /// @author arksea </summary>
    /// @param <I> Thrift服务接口 </param>
    public class PooledClientSource<I> : AbstractProxyClientSource<I>, System.IDisposable
    {

        private readonly string regname;
        private IDictionary<MsgSvcAddr, ClientPool<I>> pools = new Dictionary<MsgSvcAddr, ClientPool<I>>(); 
        private GenericObjectPoolConfig poolConfig;
        private int timeout;
        private volatile bool disposed = false;
        private MsgServiceDefine serviceInfo;
        private System.Type svcInterface;
        private FailStrategy failStrategy = FailStrategy.FAILFAST;

        /// <summary> 连接池构造函数 </summary>
        /// <param name="regname">应用服务注册名</param>
        /// <param name="cfg">连接池配置，这是一个Apache Common Pool2的实现，具体配置的定义可以参考官方文档</param>
        /// <param name="timeout">Socket通信超时时间，毫秒</param>
        public PooledClientSource(string regname, GenericObjectPoolConfig cfg, int timeout)
        {
            if (!cfg.TestOnBorrow && !cfg.TestOnReturn && !cfg.TestWhileIdle)
            {
                cfg.TestOnReturn = true;
            }
            this.regname = regname;
            this.poolConfig = cfg;
            this.timeout = timeout;
            ServiceAdaptor.subscribeService(regname);
            this.serviceInfo = ServiceManager.getServiceDefine(regname);
            string fs = this.serviceInfo.getProperty("fail_strategy");
            if (fs != null)
            {
                switch (fs)
                {
                    case "failfast":
                        this.failStrategy = FailStrategy.FAILFAST;
                        break;
                    case "failover":
                        this.failStrategy = FailStrategy.FAILOVER;
                        break;
                    case "failsafe":
                        this.failStrategy = FailStrategy.FAILSAFE;
                        break;
                }
            }
            svcInterface = typeof(I);
        }

        public override string getServiceRegname()
        {
            return regname;
        }

        private ClientPool<I> getPool(ServiceInstance inst)
        {
            lock (this)
            {
                MsgSvcAddr addr = inst.Addr;
                ClientPool<I> pool;
                if (!pools.ContainsKey(addr))
                {
                    ClientFactory<I> f = new ClientFactory<I>(serviceInfo, inst, timeout);
                    pool = new ClientPool<I>(f, poolConfig);
                    pools.Add(addr, pool);
                }
                else
                {
                    pool = pools[addr];
                }
                return pool;
            }
        }

        public override I getClient()
        {
            //if (this.serviceInfo.getProperty("route_strategy").Equals("broadcast"))
            //{
            //    return (I) BroadcastClientProxy<I>.newInstance(this, svcInterface, failStrategy);
            //}
            //else
            //{
                return  (I)ClientProxy<I>.newInstance(this, svcInterface, failStrategy);
            //}
        }

        public override void returnClient(object client)
        {
            IClientProxy p = (IClientProxy) client;
            p.Dispose();
        }

        public override RawClientInfo getRowClient()
        {
            RawClientInfo raw = new RawClientInfo();
            raw.instance = ServiceManager.getServiceInstance(regname);
            raw.client = getPool(raw.instance).borrowObject();
            return raw;
        }

        public override System.Collections.Generic.IList<RawClientInfo> getRowClients()
        {
            IList<ServiceInstance> instList = ServiceManager.getUsableServiceInstances(regname);
            System.Collections.Generic.IList<RawClientInfo> infos = new List<RawClientInfo>();
            foreach (ServiceInstance i in instList)
            {
                RawClientInfo raw = new RawClientInfo();
                raw.instance = i;
                raw.client = getPool(i).borrowObject();
                infos.Add(raw);
            }
            return infos;
        }

        public override void returnRowClient(RawClientInfo raw)
        {
            getPool(raw.instance).returnObject(raw.client);
        }


        public void Dispose()
        {
            lock (this)
            {
                disposed = true;
                foreach (ClientPool<I> e in pools.Values)
                {
                    e.Dispose();
                }
                pools.Clear();
            }
        }
    }

}