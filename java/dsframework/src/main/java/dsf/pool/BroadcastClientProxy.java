package dsf.pool;

import dsf.resource.IDisposable;
import dsf.resource.ResourceLeaksMonitor;
import dsf.route.RateLimitedException;
import java.io.IOException;
import java.lang.reflect.*;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author arksea
 */
public class BroadcastClientProxy<I> implements InvocationHandler, IClientProxy, IDisposable {

    private List<RawClientInfo> rawClients = null;
    AbstractProxyClientSource<I> clientSource;
    private final FailStrategy failStrategy;
    private volatile boolean disposed = false;
    private Throwable failedException = null;
    private final static Logger logger = LogManager.getLogger(BroadcastClientProxy.class.getName());

    private BroadcastClientProxy(AbstractProxyClientSource s, FailStrategy fs) {
        this.clientSource = s;
        this.failStrategy = fs;
        ResourceLeaksMonitor.register(this);
    }
    
    private void failedProcess(Method method, Object[] args, Throwable failEx) throws Throwable {
        if (failStrategy == FailStrategy.FAILOVER) {
            //FAILOVER策略将在轮询结束后抛出异常
            failedException = failEx;
        } else if (failStrategy == FailStrategy.FAILSAFE) {
            //FAILFAST
            //FAILSAFE策略只记录日志，不做其它处理，也不抛出异常
            logger.error("访问服务失败", failEx);
        } else {//默认作为FAILFAST策略处理
            //FAILFAST将停止轮询直接抛出异常
            throw failEx;
        }
    }
    
    @Override
    public boolean isDisposed() {
        return disposed;
    }

    @Override
    public void dispose() {
        this.disposed = true;
        for (RawClientInfo r: rawClients) {
            clientSource.returnRowClient(r);
        }
        rawClients.clear();
    }

    //用于拦截真实客户端的Closeable接口调用
    public void close() throws IOException {
        this.dispose();
    }
    
    public static Object newInstance(AbstractProxyClientSource s, Class interfac, FailStrategy fs) {
        try {
            Class proxyClass = Proxy.getProxyClass(
                    Thread.currentThread().getContextClassLoader(),
                    new Class[]{interfac, IClientProxy.class});
            Constructor con = proxyClass.getConstructor(
                    new Class[]{InvocationHandler.class});
            BroadcastClientProxy c = new BroadcastClientProxy(s,fs);
            return con.newInstance(new Object[]{c});
        } catch (Exception e) {
            throw new RuntimeException("创建资源代理类失败", e);
        }
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (isDisposed()) {
            //不允许在释放后继续使用客户端，防止出现各种奇怪的问题，比如：
            //1、Client在没有close前，用户持有客户端引用，将会使用同一个连接，
            //   这将使其业务持有session状态成为可能，如果在closed后允许继续使用
            //   将会再次到池中获取连接，这将使前面所述特性发生出乎程序员意料的变化
            //2、获取连接与归还连接是配对的行为，在Client被close后继续使用有很大的
            //   可能性忘记再次close，进而造成资源泄露
            throw new RuntimeException("客户端已经closed，不能再被使用");
        }
        String methodName = method.getName();
        if (methodName.equals("close")) {
            return method.invoke(this, args);
        } else if (methodName.equals("finalize")) {
            return method.invoke(this, args);
        } else {
            if (rawClients == null) {
                rawClients = clientSource.getRowClients();
            }
            Object ret = null;
            failedException = null;
            for (RawClientInfo rawClient: rawClients) {
                if (!rawClient.instance.getState("online").equals("true")){
                    continue;
                }
                try {
                    if (rawClient.instance.tryAcquire()) {
                        ret = method.invoke(rawClient.client, args);
                    } else {
                        throw new InvocationTargetException(
                                new RateLimitedException("rate limited, " + rawClient.instance));
                    }
                } catch (InvocationTargetException ex) {
                    rawClient.instance.onServiceFailed(ex.getCause());
                    failedProcess(method, args, ex.getCause());
                }
            }
            if (failedException != null) {
                throw failedException;
            }
            return ret;
        }
    }

}
