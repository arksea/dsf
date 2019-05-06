package dsf.pool;

import dsf.register.ServiceInstance;

/**
 * 保存原生的Thrift客户端对象及服务实例信息
 * @author arksea
 */
public class RawClientInfo<T> {

    public T client;
    public ServiceInstance instance;
}
