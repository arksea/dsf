package dsf.pool;

import dsf.register.MsgServiceDefine;
import dsf.register.ServiceInstance;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;

/**
 *
 * @author arksea
 * @param <I> Thrift服务接口
 */
public class PooledClientSource<I> extends AbstractPooledClientSource<I> {

    private Class svcInterface;
    
    public PooledClientSource(String regname, GenericObjectPoolConfig cfg, int timeout) {
        super(regname, cfg, timeout);
        try {
            svcInterface = Class.forName((new StringBuilder()).append(serviceInfo.name).append("$Interface").toString());
        } catch (ClassNotFoundException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Override
    protected Class getServiceInterface() {
        return svcInterface;
    }

    @Override
    protected ClientFactory createClientFactory(MsgServiceDefine info, final ServiceInstance inst, final int timeout) {
        return new ClientFactory(info, inst, timeout);
    }

}
