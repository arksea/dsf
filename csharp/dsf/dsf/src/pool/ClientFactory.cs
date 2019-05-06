namespace com.baidu.dsf.pool
{

    using com.baidu.dsf.register;
    using org.apache.commons.pool2;
    using org.apache.commons.pool2.impl;
    using com.baidu.dsf.thrift;
    using Thrift;
    /// <summary>
    /// thrift 客户端工厂
    /// @author arksea
    /// </summary>
    public class ClientFactory<I> : BasePooledObjectFactory<TServiceClient>
    {

        public readonly MsgServiceDefine define;
        public readonly ServiceInstance instance;
        public readonly int timeout;
        private readonly string validateMethod;
        private readonly ClientCreator<I> creator;

        public ClientFactory(MsgServiceDefine def, ServiceInstance inst, int timeout)
        {
            this.define = def;
            this.instance = inst;
            this.timeout = timeout;
            this.validateMethod = this.define.getProperty("validate");
            this.creator = new ClientCreator<I>(define);
        }

        public override TServiceClient create()
        {
            com.baidu.dsf.core.DSFLogHelper.DebugLog("ClientFactory","Create client ：" + define.regname + "@" + instance.Addr);
            return creator.create(instance, timeout);
        }

        public override bool validateObject(PooledObject<TServiceClient> p)
        {
            TServiceClient client = p.Object;
            TTagFailedTransport tt = (TTagFailedTransport) client.InputProtocol.Transport;
            return !tt.Failed && validateByMethod(client);
        }

        private bool validateByMethod(TServiceClient client)
        {
            if (validateMethod == null)
            {
                return true;
            }
            try
            {
                System.Type clientClass = typeof(I);
                System.Reflection.MethodInfo method = clientClass.GetMethod(validateMethod);
                method.Invoke(client, null);
                return true;
            }
            catch (System.Exception)
            {
                return false;
            }
        }

        public override void destroyObject(PooledObject<TServiceClient> p)
        {
            TServiceClient client = p.Object;
            client.InputProtocol.Transport.Close();
            com.baidu.dsf.core.DSFLogHelper.DebugLog("ClientFactory", "Destroy client ：" + define.regname + "@" + instance.Addr);
        }

        public override PooledObject<TServiceClient> wrap(TServiceClient obj)
        {
            return new DefaultPooledObject<TServiceClient>(obj);
        }
    }

}