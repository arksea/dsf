using System;
using System.Threading;

namespace com.baidu.dsf.server
{

    using com.baidu.dsf.adaptor;
    using com.baidu.dsf.core;
    using com.baidu.dsf.register;
    using Thrift;
    using Thrift.Protocol;
    using Thrift.Server;
    using Thrift.Transport;
    using java.util.concurrent.atomic;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    //子类需要声明ManagedResource注释：
    //@ManagedResource(objectName = "com.baidu.softquery:name=ThriftServiceManager")
    public abstract class ThriftServiceManager<T> : IServiceManager,  IThriftServerFactory, ThriftServiceManagerMXBean
    {

        private string serviceName = "not setted";
        private string version = "v1.0";

        private volatile bool isStop = false;

        //服务绑定地址
        private int serviceBindPort;
        private string serviceBindHost;
        private int managePort;

        //注册服务重试延时
        private long retryDelay = 1000;
        private long minRetryDelay = 1000;
        private long maxRetryDelay = 30000;

        //线程池配置
        private volatile int minThreadPoolThreads;
        private volatile int maxThreadPoolThreads;

        private volatile bool registered = false;
        protected internal TProcessor processor;
        private long lastStopServiceRequest = 0;

        private string dialedMethod = null;
        private bool dialedFailed = false;
        private bool dialedSucceed = false;
        private bool autoRegisterAfterAppStart = true;
        private volatile Supervisor supervisor;
        protected internal object serviceHandler;

        public virtual TProcessor Processor
        {
            set
            {
                processor = value;
            }
        }

        //DialedTask定时回调此方法，用于服务连通性测试，默认间隔30秒
        public virtual void onDialed()
        {
            if (dialedMethod == null || !registered)
            {
                return;
            }
            TTransport transport = null;
            MsgServiceReg svcreg = new MsgServiceReg(getRegname(), serviceBindHost, serviceBindPort);
            try
            {
                transport = new TSocket(serviceBindHost, serviceBindPort, 10000);
                transport.Open();
                TProtocol protocol = new TBinaryProtocol(transport);
                Type clientClass = typeof(T);
                TServiceClient client = (TServiceClient)Activator.CreateInstance(clientClass, protocol);

                System.Reflection.MethodInfo method = clientClass.GetMethod(dialedMethod);
                method.Invoke(client, new object[0]);
                if (!dialedSucceed)
                {
                    DSFLogHelper.InfoLog("ThriftServiceManager", "服务在线状态拨测成功：" + getRegname());
                    dialedSucceed = true;
                    dialedFailed = false;
                }
                try
                {
                    ServiceAdaptor.reportServiceState(svcreg, "online", "true");
                }
                catch (Exception ex)
                {
                    DSFLogHelper.WarnLog("ThriftServiceManager", ex, "报告服务状态失败");
                }
            }
            catch (Exception ex)
            {
                if (!dialedFailed)
                {
                    DSFLogHelper.WarnLog("ThriftServiceManager", ex, "服务在线状态拨测失败：" + getRegname());
                    dialedFailed = true;
                    dialedSucceed = false;
                }
                try
                {
                    ServiceAdaptor.reportServiceState(svcreg, "online", "false");
                }
                catch (Exception ex1)
                {
                    DSFLogHelper.WarnLog("ThriftServiceManager", ex, "报告服务状态失败");
                }
            }
            finally
            {
              if (transport != null)
              {
                  transport.Close();
              }
            }
        }

        private long lastRequestCount = 0L;
        private long lastStatTime = java.lang.System.currentTimeMillis();
        private readonly AtomicLong qps = new AtomicLong(0L);
        //DialedTask定时回调此方法，用于性能统计，默认间隔5秒
        public virtual void onStat()
        {
            long count = 0L;
            if (supervisor != null)
            {
                ThriftServiceTask t = (ThriftServiceTask)supervisor.getChild(getRegname());
                if (t != null)
                {
                    count = t.RequestCount;
                }
            }
            long now = java.lang.System.currentTimeMillis();
            long q = (lastRequestCount - count) * 1000 / (lastStatTime - now);
            lastRequestCount = count;
            lastStatTime = now;
            qps.set(q);
        }

        private void delayForRetry()
        {
            try
            {
                java.lang.Thread.sleep(retryDelay);
                retryDelay *= 2;
                if (retryDelay > maxRetryDelay)
                {
                    retryDelay = maxRetryDelay;
                }
            }
            catch (java.lang.InterruptedException ex)
            {
                throw new Exception("等待重试时被中断：" + getRegname(), ex);
            }
        }

        public virtual void onAppStart()
        {
            startService();
        }

        public virtual void onAppStop()
        {
            stopService(false);
        }

        public virtual void afterAppStart()
        {
            if (autoRegisterAfterAppStart)
            {
                registerService(true);
            }
        }

        public virtual void registerService()
        {
            registerService(false);
        }

        public virtual void registerService(bool retry)
        {
            MsgServiceReg svcreg = new MsgServiceReg(getRegname(), serviceBindHost, serviceBindPort);
            do
            {
                try
                {
                    ServiceAdaptor.registerService(svcreg);
                    System.Collections.Generic.IList<MsgSvcState> states = new System.Collections.Generic.List<MsgSvcState>();
                    states.Add(new MsgSvcState("manage_port", "" + managePort));
                    states.Add(new MsgSvcState("manage_protocol", "jmx"));
                    RegisterClient.setServiceState(svcreg, states);
                    TaskContext.instance().send(DialedTask.TASK_NAME, new Message("register", this));
                    DSFLogHelper.InfoLog("ThriftServiceManager", "应用服务已注册：" + getRegname());
                    retryDelay = minRetryDelay;
                    registered = true;
                    return;
                }
                catch (Exception ex)
                {
                    DSFLogHelper.WarnLog("ThriftServiceManager", ex, "注册应用服务失败：" + getRegname());
                }
                if (retry)
                {
                    delayForRetry();
                }
            } while (retry && !isStop);
        }

        public virtual void unregisterService()
        {
            unregisterService(false);
        }

        public virtual void unregisterService(bool retry)
        {
            MsgServiceReg svcreg = new MsgServiceReg(getRegname(), serviceBindHost, serviceBindPort);
            do
            {
                try
                {
                    ServiceAdaptor.unregisterService(svcreg);
                    TaskContext.instance().send(DialedTask.TASK_NAME, new Message("unregister", this));
                    DSFLogHelper.WarnLog("ThriftServiceManager", "应用服务已注销：" + getRegname());
                    retryDelay = minRetryDelay;
                    registered = false;
                    return;
                }
                catch (Exception ex)
                {
                    DSFLogHelper.WarnLog("ThriftServiceManager", ex, "注销应用服务失败：" + getRegname());
                }
                if (retry)
                {
                    delayForRetry();
                }
            } while (retry && !isStop);
        }

        public virtual void startService()
        {
            ChildInfo[] childInfos = new ChildInfo[1];
            childInfos[0] = new ChildInfo(getRegname(), typeof(ThriftServiceTask), this);
            childInfos[0].ArgsClass = typeof(IThriftServerFactory);
            supervisor = TaskContext.instance().start(getRegname(), RestartStrategies.ONE_FOR_ONE, childInfos);
            DSFLogHelper.InfoLog("ThriftServiceManager", "应用服务已启动：" + getRegname());
            isStop = false;
        }

        public virtual TServer create()
        {
            TServerTransport transport = new TServerSocket(serviceBindPort);
            return new TThreadPoolServer(processor, transport,
                                         new TTransportFactory(),
                                         new TTransportFactory(),
                                         new TBinaryProtocol.Factory(),
                                         new TBinaryProtocol.Factory(),
                                         minThreadPoolThreads,
                                         maxThreadPoolThreads,
                                         DefaultLogDelegate);
        }

        protected static void DefaultLogDelegate(string s)
        {
            Console.Error.WriteLine(s);
        }

        public virtual string stopService()
        {
            return stopService(true);
        }

        public virtual string stopService(bool confirm)
        {
            long now = java.lang.System.currentTimeMillis();

            if (confirm && (now - lastStopServiceRequest > 5000))
            {
                lastStopServiceRequest = now;
                DSFLogHelper.WarnLog("ThriftServiceManager", "收到停止应用服务的请求，5秒内再次收到此请求将执行停止服务操作：" + getRegname());
                return "警告：在5秒内再次请求将停止应用服务！";
            }
            isStop = true;
            lastStopServiceRequest = 0;
            unregisterService(false);
            DSFLogHelper.WarnLog("ThriftServiceManager", "正在停止应用服务：" + getRegname());
            TaskContext.instance().stop(getRegname());
            supervisor = null;
            return "已停止应用服务";
        }

    //----------------------------------------------------------------------
    // setter 与 getter

//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute(description = "服务注册最大重试延时时间") public long getMaxRetryDelay()
        public virtual long getMaxRetryDelay()
        {
            return maxRetryDelay;
        }

        public virtual void setMaxRetryDelay(long value)
        {
            this.maxRetryDelay = value;
        }


//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute(description = "服务注册最小重试延时时间") public long getMinRetryDelay()
        public virtual long getMinRetryDelay()
        {
            return minRetryDelay;
        }

        public virtual void setMinRetryDelay(long value)
        {
            this.minRetryDelay = value;
        }


//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute public String getServiceBindHost()
        public virtual string getServiceBindHost()
        {
            return serviceBindHost;
        }

        public virtual void setServiceBindHost(string value)
        {
                this.serviceBindHost = value;
        }


//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute public int getServiceBindPort()
        public virtual int getServiceBindPort()
        {
            return serviceBindPort;
        }

        public virtual void setServiceBindPort(int value)
        {
            this.serviceBindPort = value;
        }

        public virtual int getManagePort()
        {
            return managePort;
        }

        public virtual void setManagePort(int managePort)
        {
            this.managePort = managePort;
        }
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute public boolean isIsRegistered()
        public virtual bool isRegistered()
        {
            return registered;
        }

        public virtual int getMinThreadPoolThreads()
        {
            return minThreadPoolThreads;
        }

        public virtual void setMinThreadPoolThreads(int value)
        {
            this.minThreadPoolThreads = value;
        }

        public virtual int getMaxThreadPoolThreads()
        {
            return this.maxThreadPoolThreads;
        }

        public virtual void setMaxThreadPoolThreads(int value)
        {
            this.maxThreadPoolThreads = value;
        }


//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute public String getServiceName()
        public virtual string getServiceName()
        {
            return serviceName;
        }

        public virtual void setServiceName(string value)
        {
            this.serviceName = value;
        }


        public virtual string getVersion()
        {
            return version;
        }

        public virtual void setVersion(string value)
        {
            this.version = value;
        }


//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute(description = "服务注册名") public String getRegname()
        public virtual string getRegname()
        {
            return serviceName + " " + version;
        }


        public virtual long getRequestCount()
        {
            if (supervisor != null)
            {
                ThriftServiceTask t = (ThriftServiceTask)supervisor.getChild(getRegname());
                if (t != null)
                {
                    return t.RequestCount;
                }
            }
            return 0L;
        }

//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute(description = "每秒请求数(QPS)") public long getQPS()
        public virtual long getQPS()
        {
            return qps.get();
        }

//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute(description = "服务在线状态拨测方法") public String getDialedMethod()
        public virtual string getDialedMethod()
        {
            return dialedMethod;
        }

        public virtual void setDialedMethod(string value)
        {
            this.dialedMethod = value;
        }


//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ManagedAttribute(description = "是否在应用启动后立即注册服务") public boolean isAutoRegisterAfterAppStart()
        public virtual bool getAutoRegisterAfterAppStart()
        {
            return autoRegisterAfterAppStart;
        }

        public virtual void setAutoRegisterAfterAppStart(bool value)
        {
            this.autoRegisterAfterAppStart = value;
        }


    }

}