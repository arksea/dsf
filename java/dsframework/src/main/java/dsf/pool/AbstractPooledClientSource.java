package dsf.pool;

import dsf.adaptor.ServiceAdaptor;
import dsf.register.MsgServiceDefine;
import dsf.register.MsgSvcAddr;
import dsf.register.MsgSvcState;
import dsf.register.ServiceInstance;
import dsf.register.ServiceManager;
import dsf.resource.IDisposable;
import dsf.resource.ResourceLeaksMonitor;
import java.io.Closeable;
import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.pool2.PooledObjectFactory;
import org.apache.commons.pool2.impl.GenericObjectPoolConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author xiaohaixing_dian91
 * @param <I>
 */
public abstract class AbstractPooledClientSource<I> extends AbstractProxyClientSource<I> implements IDisposable, IServiceStateListener  {
    protected final static Logger logger = LogManager.getLogger(AbstractPooledClientSource.class);
    protected final ConcurrentHashMap<MsgSvcAddr, ClientPool> pools = new ConcurrentHashMap();
    protected GenericObjectPoolConfig poolConfig;
    protected int timeout;
    private volatile boolean disposed = false;
    protected MsgServiceDefine serviceInfo;
    protected FailStrategy failStrategy = FailStrategy.FAILFAST;

    /**
     * @param regname  应用服务注册名
     * @param cfg      连接池配置，这是一个Apache Common Pool2的实现，具体配置的定义可以参考官方文档
     * @param timeout  Socket通信超时时间,毫秒
     */
    public AbstractPooledClientSource(String regname, GenericObjectPoolConfig cfg, int timeout) {
        if (!cfg.getTestOnBorrow() && !cfg.getTestOnReturn() && !cfg.getTestWhileIdle()) {
            cfg.setTestOnReturn(true);
        }
        this.regname = regname;
        this.poolConfig = cfg;
        this.poolConfig.setJmxNamePrefix(regname);
        this.timeout = timeout;
        ServiceAdaptor.subscribeService(regname);
        ServiceManager.registerStateListener(this);
        this.serviceInfo = ServiceManager.getServiceDefine(regname);
        String fs = this.serviceInfo.getProperty("fail_strategy");
        if (fs != null) {
            if (fs.equals("failfast")) {
                    this.failStrategy = FailStrategy.FAILFAST;
            } else if (fs.equals("failover")) {
                    this.failStrategy = FailStrategy.FAILOVER;
            } else if (fs.equals("failsafe")) {
                    this.failStrategy = FailStrategy.FAILSAFE;
            }
        }
        //svcInterface = getIClass();
        ResourceLeaksMonitor.register(this);
    }

    protected abstract Class getServiceInterface();
    protected abstract PooledObjectFactory createClientFactory(final MsgServiceDefine info, final ServiceInstance inst, final int timeout);

    @Override
    public I getClient() {
        //Class svcInterface = getIClass();
        Class svcInterface = this.getServiceInterface();
        if (this.serviceInfo.getProperty("route_strategy").equals("broadcast")) {
            return (I) BroadcastClientProxy.newInstance(this, svcInterface, failStrategy);
        } else {
            return (I) ClientProxy.newInstance(this, svcInterface, failStrategy);
        }
    }
    
    public Class getIClass() {
        Class clz = getClass();
        Type genType = clz.getGenericSuperclass();  
        if (!(genType instanceof ParameterizedType)) {
            return Object.class;
        }
        Type[] params = ((ParameterizedType) genType).getActualTypeArguments();  
        if (!(params[0] instanceof Class)) {  
              return Object.class;  
        } else {
            return (Class) params[0];
        }
    }
    
    @Override
    public void returnClient(I client) {
        IClientProxy p = (IClientProxy) client;
        try {
            p.close();
        } catch (IOException ex) {
            logger.warn("close client failed", ex);
        }
    }

    private ClientPool getPool(ServiceInstance inst) {
        MsgSvcAddr addr = inst.getAddr();
        ClientPool pool = pools.get(addr);
        if (pool == null) {
            synchronized (pools) {
                pool = pools.get(addr);
                if (pool == null) {
                    PooledObjectFactory f = this.createClientFactory(serviceInfo, inst, timeout);
                    pool = new ClientPool(f, poolConfig);
                    pools.put(addr, pool);
                }
            }
        }
        return pool;
    }

    @Override
    RawClientInfo getRowClient() throws Exception {
        RawClientInfo raw = new RawClientInfo();
        raw.instance = ServiceManager.getServiceInstance(regname);
        raw.client = getPool(raw.instance).borrowObject();
        return raw;
    }

    @Override
    List<RawClientInfo> getRowClients() throws Exception {
        List<ServiceInstance> instList = ServiceManager.getUsableServiceInstances(regname);
        List<RawClientInfo> infos = new LinkedList<>();
        for (ServiceInstance i: instList) {
            RawClientInfo raw = new RawClientInfo();
            raw.instance = i;
            raw.client = getPool(i).borrowObject();
            infos.add(raw);
        }
        return infos;
    }

    @Override
    void returnRowClient(RawClientInfo raw) {
        ClientPool pool = pools.get(raw.instance.getAddr());
        if (pool == null || pool.isDisposed()) {
            try {
            ((Closeable)raw.client).close();
            } catch (IOException ex) {
                logger.error(ex);
            }
        } else {
            pool.returnObject(raw.client);
        }
    }

    @Override
    public synchronized boolean isDisposed() {
        return disposed;
    }

    @Override
    public synchronized void dispose() {
        ServiceManager.unregisterStateListener(this);
        disposed = true;
        for (Map.Entry<MsgSvcAddr, ClientPool> e : pools.entrySet()) {
            e.getValue().dispose();
        }
        pools.clear();
    }

    //根据回报的服务器状态，删除已经offline的Pool
    @Override
    public void notifySvcStates(MsgSvcAddr addr, Collection<MsgSvcState> states) {
        if (pools.containsKey(addr)) {
            for (MsgSvcState s : states) {
                if (s.name.equals("online")) {
                    if (s.value.equals("false")) {
                        ClientPool pool;
                        synchronized(pools) {
                            pool = pools.remove(addr);
                        }
                        pool.dispose();
                    }
                    break;
                }
            }            
        }
    }
    @Override
    public String getServiceRegname() {
        return regname;
    }
}
