package dsf.pool;

import dsf.register.MsgServiceDefine;
import dsf.register.ServiceInstance;
import java.io.IOException;
import org.apache.commons.pool2.PooledObjectFactory;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.apache.thrift.async.TAsyncClientManager;

/**
 *
 * @author arksea
 * @param <I> Thrift服务接口
 */
public class AsyncPooledClientSource<I> extends AbstractPooledClientSource<I> {

    private Class svcInterface;
    private TAsyncClientManager asyncClientManager;
    
    public AsyncPooledClientSource(String regname, GenericObjectPoolConfig cfg, int timeout) {
        super(regname, cfg, timeout);
        try {
            svcInterface = Class.forName((new StringBuilder()).append(serviceInfo.name).append("$AsyncInterface").toString());
            asyncClientManager = new TAsyncClientManager();
        } catch (ClassNotFoundException|IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Override
    protected Class getServiceInterface() {
        return svcInterface;
    }

    @Override
    protected PooledObjectFactory createClientFactory(MsgServiceDefine info, final ServiceInstance inst, final int timeout) {
        return new AsyncClientFactory(asyncClientManager, info, inst, timeout);
    }

}
