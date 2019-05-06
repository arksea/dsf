namespace com.baidu.dsf.pool
{

    using ServiceInstance = com.baidu.dsf.register.ServiceInstance;
    using TServiceClient = Thrift.TServiceClient;

    /// <summary>
    /// 保存原生的Thrift客户端对象及服务实例信息
    /// @author arksea
    /// </summary>
    public class RawClientInfo
    {

        public TServiceClient client;
        public ServiceInstance instance;
    }

}