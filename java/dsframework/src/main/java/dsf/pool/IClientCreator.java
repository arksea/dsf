package dsf.pool;

import dsf.register.ServiceInstance;

/**
 *
 * @author xiaohaixing_dian91
 */
public interface IClientCreator<T> {
    T createClient(ServiceInstance inst, int timeout) throws ClientSourceException;
}
