using System;
using System.Collections.Generic;
using System.Threading;

namespace com.baidu.dsf.pool
{

    using IDisposable = com.baidu.dsf.resource.IDisposable;
    using ResourceLeaksMonitor = com.baidu.dsf.resource.ResourceLeaksMonitor;
    using RateLimitedException = com.baidu.dsf.route.RateLimitedException;
    using Logger = org.slf4j.Logger;
    using LoggerFactory = org.slf4j.LoggerFactory;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class BroadcastClientProxy<I> : InvocationHandler, IClientProxy, IDisposable
    {

        private IList<RawClientInfo> rawClients = null;
        internal AbstractProxyClientSource<I> clientSource;
        private readonly FailStrategy failStrategy;
        private volatile bool disposed = false;
        private Exception failedException = null;
//JAVA TO C# CONVERTER WARNING: The .NET Type.FullName property will not always yield results identical to the Java Class.getName method:
        private static readonly Logger logger = LoggerFactory.getLogger(typeof(BroadcastClientProxy).FullName);

        private BroadcastClientProxy(IProxyClientSource s, FailStrategy fs)
        {
            this.clientSource = s;
            this.failStrategy = fs;
            ResourceLeaksMonitor.register(this);
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private void failedProcess(Method method, Object[] args, Throwable failEx) throws Throwable
        private void failedProcess(Method method, object[] args, Exception failEx)
        {
            if (failStrategy == FailStrategy.FAILOVER)
            {
                //FAILOVER策略将在轮询结束后抛出异常
                failedException = failEx;
            }
            else if (failStrategy == FailStrategy.FAILSAFE)
            {
                //FAILFAST
                //FAILSAFE策略只记录日志，不做其它处理，也不抛出异常
                logger.error("访问服务失败", failEx);
            } //默认作为FAILFAST策略处理
            else
            {
                //FAILFAST将停止轮询直接抛出异常
                throw failEx;
            }
        }

        public virtual void _proxy_returnClient()
        {
            this.dispose();
            foreach (RawClientInfo r in rawClients)
            {
                clientSource.returnRowClient(r);
            }
            rawClients.Clear();
        }

        public override bool Disposed
        {
            get
            {
                return disposed;
            }
        }

        public override void dispose()
        {
            this.disposed = true;
        }

        public static object newInstance(IProxyClientSource s, Type interfac, FailStrategy fs)
        {
            try
            {
                Type proxyClass = Proxy.getProxyClass(Thread.CurrentThread.ContextClassLoader, new Type[]{interfac, typeof(IClientProxy)});
                Constructor con = proxyClass.GetConstructor(new Type[]{typeof(InvocationHandler)});
                BroadcastClientProxy c = new BroadcastClientProxy(s,fs);
                return con.newInstance(new object[]{c});
            }
            catch (Exception e)
            {
                throw new Exception("创建资源代理类失败", e);
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
        public override object invoke(object proxy, Method method, object[] args)
        {
            string methodName = method.Name;
            if (methodName.Equals("_proxy_returnClient"))
            {
                return method.invoke(this, args);
            }
            else if (methodName.Equals("finalize"))
            {
                return method.invoke(this, args);
            }
            else
            {
                if (rawClients == null)
                {
                    rawClients = clientSource.RowClients;
                }
                object ret = null;
                failedException = null;
                foreach (RawClientInfo rawClient in rawClients)
                {
                    try
                    {
                        if (rawClient.instance.tryAcquire())
                        {
                            ret = method.invoke(rawClient.client, args);
                        }
                        else
                        {
                            throw new InvocationTargetException(new RateLimitedException("rate limited, " + rawClient.instance));
                        }
                    }
                    catch (InvocationTargetException ex)
                    {
                        rawClient.instance.onServiceFailed(ex.InnerException);
                        failedProcess(method, args, ex.InnerException);
                    }
                }
                if (failedException != null)
                {
                    throw failedException;
                }
                return ret;
            }
        }

    }

}