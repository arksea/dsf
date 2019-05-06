using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using com.baidu.dsf.core;
using com.baidu.dsf.register;
using baidu91.appsvc.demo;
using com.baidu.dsf;
using com.baidu.dsf.pool;
using com.baidu.dsf.server;
using org.apache.commons.pool2.impl;

namespace test_server
{
    using com.baidu.dsf.demo.server;
    using Thrift;
    using javax.management;
    using java.lang.management;

    internal class DemoClientManager : IServiceManager
    {
        public void onAppStart()
        {
            ChildInfo[] childInfos = new ChildInfo[1];
            for (int i = 0; i < 1; ++i)
            {
                childInfos[i] = new ChildInfo(DemoClient.TASK_NAME + i, typeof(DemoClient), new object());
            }
            TaskContext.instance().start(DemoClient.TASK_NAME, RestartStrategies.ONE_FOR_ONE, childInfos);
        }

        public void afterAppStart()
        {
        }

        public void onAppStop()
        {
            TaskContext.instance().stop(DemoClient.TASK_NAME);
        }
        public void onDialed()
        {

        }
        public void onStat()
        {

        }
        public void setManagePort(int p)
        {

        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            AppManager app = new AppManager();
            app.setAppName("dsf C# test server");
            app.setRegisterServers("192.168.253.251:9002;192.168.253.149:9002");
            app.setJmxRemotePort(9712);

            DemoSoftQueryManager serverManager = new DemoSoftQueryManager();
            serverManager.setServiceName("baidu91.appsvc.demo.DemoSoftQuery");
            serverManager.setVersion("v1.0-CSharp");
            serverManager.setDialedMethod("ping");
            serverManager.setServiceBindHost("192.168.253.184");
            serverManager.setServiceBindPort(9711);
            serverManager.setMinThreadPoolThreads(20);
            serverManager.setMaxThreadPoolThreads(50);
            DemoSoftQueryHandler handler = new DemoSoftQueryHandler();
            serverManager.Processor = new DemoSoftQuery.Processor(handler);

            app.addServiceManager(serverManager);
            app.addServiceManager(new DemoClientManager());
            app.startApplication(3000,1000);
        }
    
    }
}
