using System;
using System.Collections.Generic;

namespace com.baidu.dsf.pool
{

    using ServiceAdaptor = com.baidu.dsf.adaptor.ServiceAdaptor;
    using ServiceManager = com.baidu.dsf.register.ServiceManager;
    using MsgServiceDefine = com.baidu.dsf.register.MsgServiceDefine;
    using ServiceInstance = com.baidu.dsf.register.ServiceInstance;

    /// <summary>
    /// 短连接的IClientSource实现；连接池本身线程安全，但取得的Client非线程安全；
    /// 适合大部分应用，比如：远程方法执行时间远大于连接创建时间(大约为10毫秒级别， 看网络情况)的服务，其连接创建时间可以被忽略，此时短连接策略基本不会带来
    /// 有影响的性能损失；另外，对于非频繁调用、对延迟时间不敏感的服务也适合使用短连接策略。 当调用者获取Client时，此实现将新建一个Client并返回；
    /// 当调用者关闭Client时，此实现只是简单的直接调用其引用的Transport.close()
    /// </summary>
    /// @param <T> thrift client
    /// @author arksea / sean </param>
    public class ClientSource<I> : AbstractProxyClientSource<I>
    {

        protected string regname;
        protected int timeout;
        protected ClientCreator<I> creator;
        protected Type svcInterface;
        protected MsgServiceDefine serviceInfo;
        protected FailStrategy DEFAULT_FAIL_STRATEGY = FailStrategy.FAILFAST;

        public ClientSource(string regname, int timeout)
        {
            this.regname = regname;
            this.timeout = timeout;
            ServiceAdaptor.subscribeService(regname);
            this.serviceInfo = ServiceManager.getServiceDefine(regname);
            this.creator = new ClientCreator<I>(serviceInfo);
            svcInterface = typeof(I);
        }

        public override string getServiceRegname()
        {
            return regname;
        }

        public override I getClient()
        {
            return (I)ClientProxy<I>.newInstance(this, svcInterface, DEFAULT_FAIL_STRATEGY);
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
            raw.client = creator.create(raw.instance, timeout);
            return raw;
        }

        public override IList<RawClientInfo> getRowClients()
        {
            IList<ServiceInstance> instList = ServiceManager.getUsableServiceInstances(regname);
            IList<RawClientInfo> infos = new List<RawClientInfo>();
            foreach (ServiceInstance i in instList)
            {
                RawClientInfo raw = new RawClientInfo();
                raw.instance = i;
                raw.client = creator.create(raw.instance, timeout);
                infos.Add(raw);
            }
            return infos;
        }

        public override void returnRowClient(RawClientInfo raw)
        {
            raw.client.InputProtocol.Transport.Close();
        }
    }

}