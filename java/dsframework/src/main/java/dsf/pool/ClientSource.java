package dsf.pool;

import dsf.adaptor.ServiceAdaptor;
import dsf.register.ServiceManager;
import dsf.register.MsgServiceDefine;
import dsf.register.ServiceInstance;
import java.util.LinkedList;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.thrift.TServiceClient;

/**
 * 短连接的IClientSource实现；连接池本身线程安全，但取得的Client非线程安全；
 * 适合大部分应用，比如：远程方法执行时间远大于连接创建时间(大约为10毫秒级别， 看网络情况)的服务，其连接创建时间可以被忽略，此时短连接策略基本不会带来
 * 有影响的性能损失；另外，对于非频繁调用、对延迟时间不敏感的服务也适合使用短连接策略。 当调用者获取Client时，此实现将新建一个Client并返回；
 * 当调用者关闭Client时，此实现只是简单的直接调用其引用的Transport.close()
 *
 * @author arksea / sean
 */
public class ClientSource<I> extends AbstractProxyClientSource<I> {

    protected int timeout;
    protected ClientCreator creator;
    protected Class svcInterface;
    protected MsgServiceDefine serviceInfo;
    protected FailStrategy DEFAULT_FAIL_STRATEGY = FailStrategy.FAILFAST;
    private final static Logger logger = LogManager.getLogger(ClientSource.class);
    /**
     * @param regname  应用服务注册名
     * @param timeout  Socket通信超时时间
     */
    public ClientSource(String regname, int timeout) {
        this.regname = regname;
        this.timeout = timeout;
        ServiceAdaptor.subscribeService(regname);
        this.serviceInfo = ServiceManager.getServiceDefine(regname);
        this.creator = new ClientCreator(serviceInfo);
        try {
            svcInterface = Class.forName(serviceInfo.name + "$Interface");
        } catch (ClassNotFoundException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Override
    public I getClient() {
        return (I) ClientProxy.newInstance(this, svcInterface, DEFAULT_FAIL_STRATEGY);
    }

    @Override
    public void returnClient(I client) {
        IClientProxy p = (IClientProxy) client;
        try{
            p.close();
        } catch (Exception ex) {
            logger.warn("close client failed", ex);
        }
    }

    @Override
    public RawClientInfo getRowClient() throws Exception {
        RawClientInfo raw = new RawClientInfo();
        raw.instance = ServiceManager.getServiceInstance(regname);
        raw.client = creator.createClient(raw.instance, timeout);
        return raw;
    }

    @Override
    public List<RawClientInfo> getRowClients() throws Exception {
        List<ServiceInstance> instList = ServiceManager.getUsableServiceInstances(regname);
        List<RawClientInfo> infos = new LinkedList<RawClientInfo>();
        for (ServiceInstance i: instList) {
            RawClientInfo raw = new RawClientInfo();
            raw.instance = i;
            raw.client = creator.createClient(raw.instance, timeout);
            infos.add(raw);
        }
        return infos;
    }

    @Override
    public void returnRowClient(RawClientInfo raw) {
        ((TServiceClient)raw.client).getInputProtocol().getTransport().close();
    }
}
