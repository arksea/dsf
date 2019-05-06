using System;

namespace com.baidu.dsf.pool
{
    using Thrift.Protocol;
    using Thrift.Transport;
    using com.baidu.dsf.register;
    using com.baidu.dsf.thrift;
    using Thrift;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class ClientCreator<I>
    {
        private readonly MsgServiceDefine serviceInfo;

        public ClientCreator(MsgServiceDefine serviceInfo)
        {
            this.serviceInfo = serviceInfo;
        }

        public virtual TServiceClient create(ServiceInstance inst, int timeout)
        {
            TSocket sock = null;
            MsgSvcAddr svcAddr = inst.Addr;
            try
            {
                sock = new TSocket(svcAddr.host, svcAddr.port, timeout);
                sock.Open();
                string protocolStr = serviceInfo.getProperty("protocol");
                if (protocolStr == null)
                {
                    throw new ApplicationException("service not defined protocol: " + serviceInfo.regname);
                }
                TProtocol protocol = ProtocolFactory.create(protocolStr, sock);
                string className = serviceInfo.name + "+Client";
                System.Reflection.Assembly assembly =System.Reflection.Assembly.GetAssembly(typeof(I));
                Type clientClass = assembly.GetType(className);
                var client = (TServiceClient)Activator.CreateInstance(clientClass, protocol);
                inst.onConnectSucceed();
                return client;
            }
            catch (Exception ex)
            {
                if (sock != null)
                {
                    sock.Close();
                }
                inst.onConnectFailed(ex);
                string err = "Create client failed：" + serviceInfo.regname + "@" + svcAddr.host + ":" + svcAddr.port;
                throw new ClientSourceException(err, serviceInfo.regname, svcAddr, ex);
            }
        }
    }

}