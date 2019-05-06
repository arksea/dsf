package dsf.pool;

import dsf.register.MsgServiceDefine;
import dsf.register.MsgSvcAddr;
import org.apache.commons.pool2.BasePooledObjectFactory;
import org.apache.commons.pool2.PooledObject;
import org.apache.commons.pool2.impl.DefaultPooledObject;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import dsf.register.ServiceInstance;
import java.io.Closeable;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import org.apache.thrift.async.TAsyncClient;
import org.apache.thrift.async.TAsyncClientManager;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransport;

/**
 * thrift 客户端工厂
 * @author arksea
 */
public class AsyncClientFactory extends BasePooledObjectFactory<TAsyncClient> {

    private final static Logger logger = LogManager.getLogger(AsyncClientFactory.class);
    public final MsgServiceDefine define;
    public final ServiceInstance instance;
    public final int timeout;
    //private final Method validateMethod;
    private final TAsyncClientManager asyncClientManager;

    public AsyncClientFactory(TAsyncClientManager asyncClientManager, MsgServiceDefine def, ServiceInstance inst, int timeout) {
        this.asyncClientManager = asyncClientManager;
        this.define = def;
        this.instance = inst;
        this.timeout = timeout;
//        String str = this.define.getProperty("validate");
//        String methodName = str==null ? "ping" : str;
//        Method method = null;
//        try {
//            String className = define.name + "$AsyncClient";
//            Class<?> clientClass = Class.forName(className);
//            method = clientClass.getMethod(methodName);
//            logger.info("Service '{}' use Method {} validate pooled clients", define.name, methodName);
//        } catch(ClassNotFoundException | NoSuchMethodException | SecurityException ex) {
//            logger.warn("Service '{}' has't client validate Method", ex, define.name);
//        }
//        this.validateMethod = method;
    }

    @Override
    public TAsyncClient create() throws Exception {
        logger.debug("Create client of Service {}@{}", define.regname, instance.getAddr());
        return createClient(instance, timeout);
    }

    @Override
    public boolean validateObject(PooledObject<TAsyncClient> p) {
        TAsyncClient client = p.getObject();
        boolean hasError = ((TAsyncClient) client).hasError();
        return !hasError;
    }
    
    @Override
    public void destroyObject(PooledObject<TAsyncClient> p) throws Exception {
        logger.debug("Destroy the invalidated client of Service {}@{}", define.regname, instance.getAddr());
        TAsyncClient client = p.getObject();
        ((Closeable)client).close();
    }

    @Override
    public PooledObject<TAsyncClient> wrap(TAsyncClient obj) {
        return new DefaultPooledObject<>(obj);
    }
    
    private TAsyncClient createClient(ServiceInstance inst, int timeout) throws ClientSourceException {
        TTransport transport = null;
        MsgSvcAddr svcAddr = inst.getAddr();
        try {
            transport = new TNonblockingSocket(svcAddr.host, svcAddr.port, timeout);
            String protocolStr = define.getProperty("protocol");
            assert (protocolStr != null);
            //TProtocolFactory protocolFactory = ProtocolFactoryUtil.getFactory(protocolStr);
            TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();
            String className = define.name + "$AsyncClient";
            Class<?> clientClass = Class.forName(className);
            Constructor con = clientClass.getConstructor(TProtocolFactory.class, TAsyncClientManager.class, TNonblockingTransport.class);
            TAsyncClient client = (TAsyncClient) con.newInstance(protocolFactory, asyncClientManager, transport);
            inst.onConnectSucceed();
            return client;
        } catch (Throwable ex) {
            if (transport != null) {
                transport.close();
            }
            inst.onConnectFailed(ex);
            String err = "Create client failed：" + define.regname + "@" + svcAddr.host + ":" + svcAddr.port;
            logger.debug(err, ex);
            throw new ClientSourceException(err, define.regname, svcAddr, ex);
        }
    }
}
