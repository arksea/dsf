namespace com.baidu.dsf.pool
{
    using com.baidu.dsf.register;
    using Thrift;
    using DynamicProxy;
    using com.baidu.dsf.core;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class ClientProxy<I> : IProxyInvocationHandler, IClientProxy
    {

        private RawClientInfo rawClient;
        internal AbstractProxyClientSource<I> clientSource;
        private readonly FailStrategy failStrategy;
        private volatile bool disposed = false;

        private ClientProxy(AbstractProxyClientSource<I> s, FailStrategy fs)
        {
            this.clientSource = s;
            this.failStrategy = fs;
        }

        private object failedProcess(System.Reflection.MethodInfo method, object[] args, System.Exception failEx)
        {
            string regname = clientSource.getServiceRegname();
            if (failStrategy == FailStrategy.FAILOVER)
            {
                //FAILOVER策略再获取一个服务实例，重试一遍
                clientSource.returnRowClient(rawClient);
                rawClient = null;
                rawClient = clientSource.getRowClient();
                try
                {
                    long a = java.lang.System.currentTimeMillis();
                    object ret = invokeClient(method, args);
                    long rt = java.lang.System.currentTimeMillis() - a;
                    rawClient.instance.onServiceSucceed(rt);
                    return ret;
                }
                catch (System.Reflection.TargetException ex)
                {
                    throw ex;
                }
                catch (System.ArgumentException ex)
                {
                    throw ex;
                }
                catch (System.Reflection.TargetInvocationException ex)
                {
                    throw ex;
                }
                catch (System.Reflection.TargetParameterCountException ex)
                {
                    throw ex;
                }
                catch (System.MethodAccessException ex)
                {
                    throw ex;
                }
                catch (System.InvalidOperationException ex)
                {
                    throw ex;
                }
                catch (System.NotSupportedException ex)
                {
                    throw ex;
                }
                catch (System.Exception ex)
                {
                    rawClient.instance.onServiceFailed(ex);
                    throw ex;
                }
            }
            else if (failStrategy == FailStrategy.FAILSAFE)
            {
                //FAILSAFE策略只记录日志，不做其它处理，也不抛出异常
                DSFLogHelper.ErrorLog("ClientProxy" ,failEx, "访问服务失败");
                
                return null;
            } //默认作为FAILFAST策略处理
            else
            {
                //FAILFAST策略直接抛出异常，不作其它处理
                throw failEx;
            }
        }

        public void Dispose()
        {
            this.disposed = true;
            if (rawClient != null)
            {
                clientSource.returnRowClient(rawClient);
                rawClient = null;
            }
        }

        public static object newInstance(AbstractProxyClientSource<I> s, System.Type objType, FailStrategy fs)
        {
            return ProxyFactory.GetInstance().Create(
                new ClientProxy<I>(s, fs), objType, true, new System.Type[]{typeof(IClientProxy)});

        }

        public object Invoke(object proxy, System.Reflection.MethodInfo method, object[] args)
        {
            if (this.disposed)
            {
                //不允许在释放后继续使用客户端，防止出现各种奇怪的问题，比如：
                //1、Client在没有close前，用户持有客户端引用，将会使用同一个连接，
                //   这将使其业务持有session状态成为可能，如果在closed后允许继续使用
                //   将会再次到池中获取连接，这将使前面所述特性发生出乎程序员意料的变化
                //2、获取连接与归还连接是配对的行为，在Client被close后继续使用有很大的
                //   可能性忘记再次close，进而造成资源泄露
                throw new System.ApplicationException("客户端已经closed，不能再被使用");
            }
            string methodName = method.Name;
            if (methodName.Equals("Dispose"))
            {
                return method.Invoke(this, args);
            }
            else if (methodName.Equals("finalize"))
            {
                return method.Invoke(this, args);
            }
            else
            {
                if (rawClient == null)
                {
                    rawClient = clientSource.getRowClient();
                }
                try
                {
                    long a = java.lang.System.currentTimeMillis();
                    object ret = invokeClient(method, args);
                    long rt = java.lang.System.currentTimeMillis() - a;
                    rawClient.instance.onServiceSucceed(rt);
                    return ret;
                }
                catch (System.Reflection.TargetException ex)
                {
                    throw ex;
                }
                catch (System.ArgumentException ex)
                {
                    throw ex;
                }
                catch (System.Reflection.TargetInvocationException ex)
                {
                    throw ex;
                }
                catch (System.Reflection.TargetParameterCountException ex)
                {
                    throw ex;
                }
                catch (System.MethodAccessException ex)
                {
                    throw ex;
                }
                catch(System.InvalidOperationException ex)
                {
                    throw ex;
                }
                catch (System.NotSupportedException ex)
                {
                    throw ex;
                }
                catch (System.Exception ex)
                {
                    rawClient.instance.onServiceFailed(ex);
                    return failedProcess(method, args, ex);
                }
            }
        }

        private object invokeClient(System.Reflection.MethodInfo method, object[] args)
        {
            return method.Invoke(rawClient.client, args);
        }
    }

}