package dsf.pool;

import dsf.resource.IDisposable;
import dsf.resource.ResourceLeaksMonitor;
import java.lang.reflect.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 *
 * @author arksea
 */
public class ClientProxy<I> implements InvocationHandler, IClientProxy, IDisposable {

    private RawClientInfo rawClient;
    AbstractProxyClientSource<I> clientSource;
    private final FailStrategy failStrategy;
    private volatile boolean disposed = false;
    private final static Logger logger = LogManager.getLogger(ClientProxy.class.getName());

    private ClientProxy(AbstractProxyClientSource s, FailStrategy fs) {
        this.clientSource = s;
        this.failStrategy = fs;
        ResourceLeaksMonitor.register(this);
    }

    private Object failedProcess(Method method, Object[] args, Throwable failEx) throws Throwable {
        if (failStrategy == FailStrategy.FAILOVER) {
            //FAILOVER策略再获取一个服务实例，重试一遍
            clientSource.returnRowClient(rawClient);
            rawClient = null;
            rawClient = clientSource.getRowClient();
            return invokeClient(method, args);
        } else if (failStrategy == FailStrategy.FAILSAFE) {
            //FAILSAFE策略只记录日志，不做其它处理，也不抛出异常
            logger.error("访问服务失败", failEx);
            return null;
        } else {//默认作为FAILFAST策略处理
            //FAILFAST策略直接抛出异常，不作其它处理
            throw failEx;
        }
    }

    //用于资源泄露检测
    @Override
    public boolean isDisposed() {
        return disposed;
    }

    @Override
    public synchronized void dispose() {
        this.disposed = true;
        if (rawClient != null) {
            clientSource.returnRowClient(rawClient);
            rawClient = null;
        }
    }

    //用于拦截真实客户端的Closeable接口调用
    @Override
    public void close() {
        this.dispose();
    }

    public static Object newInstance(AbstractProxyClientSource s, Class interfac, FailStrategy fs) {
        try {
            Class proxyClass = Proxy.getProxyClass(
                    Thread.currentThread().getContextClassLoader(),
                    new Class[]{interfac, IClientProxy.class});
            Constructor con = proxyClass.getConstructor(
                    new Class[]{InvocationHandler.class});
            return con.newInstance(new Object[]{new ClientProxy(s, fs)});
        } catch (Exception e) {
            throw new RuntimeException("创建资源代理类失败", e);
        }
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        String methodName = method.getName();
        if (methodName.equals("close")) {
            return method.invoke(this, args);
        } else if (isDisposed()) {
            //不允许在释放后继续使用客户端，防止出现各种奇怪的问题，比如：
            //1、Client在没有close前，用户持有客户端引用，将会使用同一个连接，
            //   这将使其业务持有session状态成为可能，如果在closed后允许继续使用
            //   将会再次到池中获取连接，这将使前面所述特性发生出乎程序员意料的变化
            //2、获取连接与归还连接是配对的行为，在Client被close后继续使用有很大的
            //   可能性忘记再次close，进而造成资源泄露
            throw new RuntimeException("客户端已经closed，不能再被使用");
        } else if (methodName.equals("finalize")) {
            return method.invoke(this, args);
        } else {
            if (rawClient == null) {
                rawClient = clientSource.getRowClient();
            }
            return invokeClient(method, args);
        }
    }
    private synchronized Object invokeClient(Method method, Object[] args) throws Throwable {
        try {
            long a = System.currentTimeMillis();
            Object ret = method.invoke(rawClient.client, args);
            long rt = System.currentTimeMillis() - a;
            rawClient.instance.onServiceSucceed(rt);
            return ret;
        } catch (InvocationTargetException ex) {
            rawClient.instance.onServiceFailed(ex.getCause());
            return failedProcess(method, args, ex.getCause());
        }
    }
}
