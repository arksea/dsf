using sun.management;
namespace com.baidu.dsf.server
{

    using com.baidu.dsf.core;
    using javax.management;
    using java.lang.management;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class AppManager : com.baidu.dsf.server.AppManagerMXBean
    {
        private bool InstanceFieldsInitialized = false;
        private int jmxRemotePort = 9999;

        public AppManager()
        {
            if (!InstanceFieldsInitialized)
            {
                InitializeInstanceFields();
                InstanceFieldsInitialized = true;
            }
        }

        private void InitializeInstanceFields()
        {
            appstoper = new AppStopper(this);
        }

        private string registerServers;

        //登录名
        private string appName;
        private long lastStopAppRequest = 0;
        private java.util.LinkedList serviceList = new java.util.LinkedList();

        //从类似192.168.253.12:1033的配置串中提取host与port，并创建InetSocketAddress列表
        public static java.net.InetSocketAddress[] parseSocketAddrArray(string cfgStr)
        {
            string[] cfgList = cfgStr.Split(';');
            int count = cfgList.Length;
            java.net.InetSocketAddress[] addrs = new java.net.InetSocketAddress[count];
            for (int i = 0; i < count; ++i)
            {
                string cfg = cfgList[i];
                string[] strs = cfg.Split(':');
                string host = strs[0];
                int port = System.Convert.ToInt32(strs[1]);
                addrs[i] = new java.net.InetSocketAddress(host, port);
            }
            return addrs;
        }
        public virtual void startApplication()
        {
            startApplication(3000,0);
        }

        public virtual void startApplication(long registryDelayMillis, long serviceDelayMillis)
        {

            DSFLogHelper.InfoLog("AppManager","启动应用进程: " + appName);

            //启动JMX远程访问接口
            java.lang.System.setProperty("com.sun.management.jmxremote", "true");
            java.lang.System.setProperty("com.sun.management.jmxremote.authenticate", "false");
            java.lang.System.setProperty("com.sun.management.jmxremote.ssl", "false");
            java.lang.System.setProperty("com.sun.management.jmxremote.port", ""+jmxRemotePort);
            Agent.startAgent();

            MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
            ObjectName name = new ObjectName("com.baidu.dsf:name=AppManager");
            mbs.registerMBean(new StandardMBean(this, typeof(AppManagerMXBean)), name);

            java.net.InetSocketAddress[] addrs = parseSocketAddrArray(registerServers);
            DSFramework.start(addrs, appName, 30);

            ChildInfo[] childInfos = new ChildInfo[1];
            object args = DialedTask.createArgs();
            childInfos[0] = new ChildInfo(DialedTask.TASK_NAME, typeof(DialedTask), args);
            childInfos[0].ArgsClass = typeof(System.Collections.Generic.IList<IServiceManager>);
            TaskContext.instance().start(DialedTask.TASK_NAME, RestartStrategies.ONE_FOR_ONE, childInfos);

            java.util.Iterator it = serviceList.iterator();
            while (it.hasNext())
            {
                IServiceManager s = (IServiceManager)it.next();
                java.lang.Thread.sleep(serviceDelayMillis);
                s.onAppStart();
            }
            java.lang.Thread.sleep(registryDelayMillis);
            it = serviceList.iterator();
            while (it.hasNext())
            {
                IServiceManager s = (IServiceManager)it.next();
                java.lang.Thread.sleep(serviceDelayMillis);
                s.afterAppStart();

                if (s is ThriftServiceManagerMXBean)
                {
                    ThriftServiceManagerMXBean bean = (ThriftServiceManagerMXBean)s;
                    ObjectName n = new ObjectName("com.baidu.dsf:name="+bean.getServiceName());
                    mbs.registerMBean(new StandardMBean(bean, typeof(ThriftServiceManagerMXBean)), n);
                }
            }
        }
        internal class AppStopper : java.lang.Runnable
        {
            private readonly AppManager outerInstance;

            public AppStopper(AppManager outerInstance)
            {
                this.outerInstance = outerInstance;
            }

            public void run()
            {
                try
                {
                    java.lang.Thread.sleep(3000);
                    //停止服务时用启动时的倒序
                    for (int i = outerInstance.serviceList.size() - 1; i >= 0; --i)
                    {
                        IServiceManager s = (IServiceManager)outerInstance.serviceList.get(i);
                        s.onAppStop();
                    }
                    java.lang.Thread.sleep(3000);
                    DSFramework.stop();
                    java.lang.Thread.sleep(30000);
                    java.lang.System.exit(0);
                }
                catch (System.Exception ex)
                {
                    DSFLogHelper.WarnLog("AppManager", ex, "退出应用进程时出错");
                }
            }
        }
        private AppStopper appstoper;

        //退出服务进程
        public virtual string stopApplication()
        {
            long now = java.lang.System.currentTimeMillis();
            if (now - lastStopAppRequest > 5000)
            {
                lastStopAppRequest = now;
                DSFLogHelper.WarnLog("AppManager", "收到退出应用进程的请求，5秒内再次收到此请求将执行退出进程操作");
                return "警告：在5秒内再次请求将退出应用进程！";
            }
            lastStopAppRequest = 0;
            DSFLogHelper.WarnLog("AppManager", "正在退出应用进程: " + appName);

            java.lang.Thread stopThread = new java.lang.Thread(appstoper);
            stopThread.setDaemon(true);
            stopThread.start();
            return "正在退出应用进程";
        }

        public virtual void addServiceManager(IServiceManager s)
        {
            s.setManagePort(jmxRemotePort);
            serviceList.Add(s);
        }

    //----------------------------------------------------------------------
    // setter 与 getter
        public virtual string getRegisterServers()
        {
            return this.registerServers;
        }

        public virtual void setRegisterServers(string addrs)
        {
                this.registerServers = addrs;
        }


        public virtual string getAppName()
        {
            return appName;
        }

        public virtual void setAppName(string name)
        {
                this.appName = name;
        }

        public void setJmxRemotePort(int port)
        {
            this.jmxRemotePort = port;
        }
    }

}