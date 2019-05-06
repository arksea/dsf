package dsf.pool;

import dsf.register.MsgServiceDefine;
import dsf.register.MsgSvcAddr;
import org.apache.commons.pool2.BasePooledObjectFactory;
import org.apache.commons.pool2.PooledObject;
import org.apache.commons.pool2.impl.DefaultPooledObject;
import org.apache.thrift.TServiceClient;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import dsf.thrift.TTagFailedTransport;
import dsf.register.ServiceInstance;
import dsf.thrift.ProtocolFactoryUtil;
import java.io.Closeable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;

/**
 * thrift 客户端工厂
 * @author arksea
 */
public class ClientFactory extends BasePooledObjectFactory<TServiceClient> {

    private final static Logger logger = LogManager.getLogger(ClientFactory.class);
    public final MsgServiceDefine define;
    public final ServiceInstance instance;
    public final int timeout;
    private final Method validateMethod;

    public ClientFactory(MsgServiceDefine def, ServiceInstance inst, int timeout) {
        this.define = def;
        this.instance = inst;
        this.timeout = timeout;
        String str = this.define.getProperty("validate");
        String methodName = str==null ? "ping" : str;
        Method method = null;
        try {
            String className = define.name + "$Client";
            Class<?> clientClass = Class.forName(className);
            method = clientClass.getMethod(methodName);
            logger.info("Service '{}' use Method {} validate pooled clients", define.name, methodName);
        } catch(ClassNotFoundException | NoSuchMethodException | SecurityException ex) {
            logger.warn("Service '{}' has't client validate Method", ex, define.name);
        }
        this.validateMethod = method;
    }

    @Override
    public TServiceClient create() throws Exception {
        logger.debug("Create client of Service {}@{}", define.regname, instance.getAddr());
        return createClient(instance, timeout);
    }

    @Override
    public boolean validateObject(PooledObject<TServiceClient> p) {
        TServiceClient client = p.getObject();
        TTagFailedTransport tt = (TTagFailedTransport) ((TServiceClient)client).getInputProtocol().getTransport();
        return !tt.isFailed() && validateByMethod(client);

    }

    private boolean validateByMethod(TServiceClient client) {
        if (validateMethod == null) return true;
        try {
            validateMethod.invoke(client);
            return true;
        } catch(IllegalAccessException | IllegalArgumentException ex) {
            logger.warn("Service {} can not access validate method {}",define.regname,validateMethod.getName(), ex);
            return true;
        } catch(InvocationTargetException ex) {
            logger.debug("The client of Service {} has invalidated",define.regname,ex);
            return false;
        }
    }
    
    @Override
    public void destroyObject(PooledObject<TServiceClient> p) throws Exception {
        logger.debug("Destroy the invalidated client of Service {}@{}", define.regname, instance.getAddr());
        TServiceClient client = p.getObject();
        ((Closeable)client).close();
    }

    @Override
    public PooledObject<TServiceClient> wrap(TServiceClient obj) {
        return new DefaultPooledObject<>(obj);
    }
    
    private TServiceClient createClient(ServiceInstance inst, int timeout) throws ClientSourceException {
        TSocket sock = null;
        MsgSvcAddr svcAddr = inst.getAddr();
        try {
            sock = new TSocket(svcAddr.host, svcAddr.port, timeout);
            sock.open();
            String protocolStr = define.getProperty("protocol");
            assert (protocolStr != null);
            TProtocol protocol = ProtocolFactoryUtil.getFactory(protocolStr).getProtocol(sock);
            String className = define.name + "$Client";
            Class<?> clientClass = Class.forName(className);
            Constructor con = clientClass.getConstructor(TProtocol.class);
            TServiceClient client = (TServiceClient) con.newInstance(protocol);
            inst.onConnectSucceed();
            return client;
        } catch (Throwable ex) {
            if (sock != null) {
                sock.close();
            }
            inst.onConnectFailed(ex);
            String err = "Create client failed：" + define.regname + "@" + svcAddr.host + ":" + svcAddr.port;
            logger.debug(err, ex);
            throw new ClientSourceException(err, define.regname, svcAddr, ex);
        }
    }
}
