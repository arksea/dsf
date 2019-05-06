using System;

namespace com.baidu.dsf
{
    using ChildInfo = com.baidu.dsf.core.ChildInfo;
    using RestartStrategies = com.baidu.dsf.core.RestartStrategies;
    using TaskContext = com.baidu.dsf.core.TaskContext;
    using RegisterClient = com.baidu.dsf.register.RegisterClient;
    using ServiceManager = com.baidu.dsf.register.ServiceManager;
    using java.net;
    using com.baidu.dsf.core;

    /// <summary>
    /// 服务框架
    /// @author arksea / sean
    /// </summary>
    public class DSFramework
    {

        private static readonly DSFramework app = new DSFramework();
        private static bool started = false;

        private DSFramework()
        {
        }

        /// <summary>
        /// 启动框架 </summary>
        /// <param name="addrs">                     注册服务器地址列表 </param>
        /// <param name="loginName">                 注册服务器登录名，作客户端标示 </param>
        public static void start(InetSocketAddress[] addrs, string loginName)
        {
            lock (typeof(DSFramework))
            {
                start(addrs, loginName, 30);
            }
        }

        /// <summary>
        /// 启动框架 </summary>
        /// <param name="addrs">                     注册服务器地址列表 </param>
        /// <param name="loginName">                 注册服务器登录名，作客户端标示 </param>
        /// <param name="maxQueueSize">              最大消息队列长度 </param>
        public static void start(InetSocketAddress[] addrs, string loginName, int maxQueueSize)
        {
            lock (typeof(DSFramework))
            {
                DSFLogHelper.DebugLog("DSFramework","DSFramwork starting");
                app.onStart(addrs, loginName, maxQueueSize);
                DSFramework.started = true;
                DSFLogHelper.DebugLog("DSFramework", "DSFramwork started");
            }
        }

        private void onStart(InetSocketAddress[] addrs, string loginName, int maxQueueSize)
        {
            if (addrs == null || addrs.Length == 0)
            {
                throw new Exception("注册服务器列表不能为空");
            }
            if (loginName == null || loginName.Length == 0)
            {
                throw new Exception("注册服务器登录名不能为空");
            }

            TaskContext.instance().stopAll();
            ChildInfo[] childs = new ChildInfo[2];

            // 注册服务器客户端线程
            childs[0] = RegisterClient.createChildInfo(loginName, addrs, maxQueueSize);
            // 服务管理线程
            childs[1] = ServiceManager.createChildInfo(maxQueueSize);
            TaskContext.instance().start("register_sup", RestartStrategies.ONE_FOR_ONE, childs);
        }

        public static void stop()
        {
            lock (typeof(DSFramework))
            {
                DSFLogHelper.DebugLog("DSFramework","DSFramwork stopping");
                TaskContext.instance().stopAll();
                DSFramework.started = false;
                DSFLogHelper.DebugLog("DSFramework","DSFramwork stopped");
            }
        }

        public static bool Started
        {
            get
            {
                lock (typeof(DSFramework))
                {
                    return DSFramework.started;
                }
            }
        }
    }

}