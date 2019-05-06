package dsf.pool;

import java.lang.reflect.Constructor;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import dsf.register.MsgServiceDefine;
import dsf.register.MsgSvcAddr;
import dsf.register.ServiceInstance;
import dsf.thrift.ProtocolFactoryUtil;
import java.lang.reflect.Method;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.async.TAsyncClient;
import org.apache.thrift.async.TAsyncClientManager;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransport;

/**
 *
 * @author arksea
 */
public class AsyncClientCreator implements IClientCreator<TAsyncClient> {

    private final static Logger logger = LogManager.getLogger(ClientFactory.class);
    private final MsgServiceDefine serviceInfo;
    private final TAsyncClientManager asyncClientManager;

    public AsyncClientCreator(MsgServiceDefine serviceInfo, TAsyncClientManager asyncClientManager) {
        this.serviceInfo = serviceInfo;
        this.asyncClientManager = asyncClientManager;
    }
 
    @Override
    public TAsyncClient createClient(ServiceInstance inst, int timeout) throws ClientSourceException {
        TTransport transport = null;
        MsgSvcAddr svcAddr = inst.getAddr();
        try {
            transport = new TNonblockingSocket(svcAddr.host, svcAddr.port, timeout);
            String protocolStr = serviceInfo.getProperty("protocol");
            assert (protocolStr != null);
            //TProtocolFactory protocolFactory = ProtocolFactoryUtil.getFactory(protocolStr);
            TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();
            String className = serviceInfo.name + "$AsyncClient";
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
            String err = "Create client failed：" + serviceInfo.regname + "@" + svcAddr.host + ":" + svcAddr.port;
            logger.debug(err, ex);
            throw new ClientSourceException(err, serviceInfo.regname, svcAddr, ex);
        }
    }

}
