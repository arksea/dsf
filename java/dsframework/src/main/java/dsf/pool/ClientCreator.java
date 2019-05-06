package dsf.pool;

import java.lang.reflect.Constructor;

import org.apache.thrift.TServiceClient;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TSocket;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import dsf.register.MsgServiceDefine;
import dsf.register.MsgSvcAddr;
import dsf.register.ServiceInstance;
import dsf.thrift.ProtocolFactoryUtil;

/**
 *
 * @author arksea
 */
public class ClientCreator implements IClientCreator<TServiceClient> {

    private final static Logger logger = LogManager.getLogger(ClientFactory.class);
    private final MsgServiceDefine serviceInfo;

    public ClientCreator(MsgServiceDefine serviceInfo) {
        this.serviceInfo = serviceInfo;
    }

    @Override
    public TServiceClient createClient(ServiceInstance inst, int timeout) throws ClientSourceException {
        TSocket sock = null;
        MsgSvcAddr svcAddr = inst.getAddr();
        try {
            sock = new TSocket(svcAddr.host, svcAddr.port, timeout);
            sock.open();
            String protocolStr = serviceInfo.getProperty("protocol");
            assert (protocolStr != null);
            TProtocol protocol = ProtocolFactoryUtil.getFactory(protocolStr).getProtocol(sock);
            String className = serviceInfo.name + "$Client";
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
            String err = "Create client failed：" + serviceInfo.regname + "@" + svcAddr.host + ":" + svcAddr.port;
            logger.debug(err, ex);
            throw new ClientSourceException(err, serviceInfo.regname, svcAddr, ex);
        }
    }
}
