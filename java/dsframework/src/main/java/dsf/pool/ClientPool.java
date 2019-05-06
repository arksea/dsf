package dsf.pool;

import org.apache.commons.pool2.impl.GenericObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import dsf.resource.IDisposable;
import org.apache.commons.pool2.PooledObjectFactory;

/**
 * 连接池
 * @author arksea
 */
public class ClientPool extends GenericObjectPool implements IDisposable {

    public ClientPool(PooledObjectFactory factory, GenericObjectPoolConfig cfg) {
        super(factory, cfg);
    }

    @Override
    public boolean isDisposed() {
        return this.isClosed();
    }

    @Override
    public void dispose() {
        if (!this.isClosed()) {
            this.close();
        }
    }
//    public String getServiceRegname() {
//        ClientFactory f = (ClientFactory) getFactory();
//        return f.define.regname;
//    }
}
