using System;

namespace com.baidu.dsf.demo.server
{

    using baidu91.appsvc.demo;
    using com.baidu.dsf.core;
    using com.baidu.dsf.pool;
    using org.apache.commons.pool2.impl;

    /// 
    /// <summary>
    /// @author xhx
    /// </summary>
    public class DemoClient : MessageChildTask
    {
        public const string TASK_NAME = "demo_client";
        private PooledClientSource<DemoSoftQuery.Interface> pool;
        public DemoClient(string name, int maxQueueLen, object args) : base(name, maxQueueLen, args)
        {
        }

        protected override void handle_info(Message msg, string from)
        {
            if (msg.name.Equals("request"))
            {
                try
                {
                    test();
                    TaskContext.instance().send_after(25, getName(), new Message("request", "ok"));
                }
                catch (Exception ex)
                {
                    DSFLogHelper.ErrorLog("DemoClient", ex, "test call failed");
                    TaskContext.instance().send_after(30000, getName(), new Message("request", "ok"));
                }
            }
        }

        public virtual void test()
        {
            using (DemoSoftQuery.Interface client = pool.getClient())
            {
                //SimpleSoft s = client.getByID(1234);
                System.Collections.Generic.List<SimpleSoft> all = client.getAll();
                System.Console.WriteLine("get count = "+all.Count);
            }
        }

        protected override Message handle_call(Message msg, string from)
        {
            throw new System.NotSupportedException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

        protected override void init()
        {
            GenericObjectPoolConfig cfg = new GenericObjectPoolConfig();
            cfg.MinIdle = 1;
            cfg.MaxTotal = 3;
            cfg.TestOnBorrow = true;
            //cfg.setTestOnReturn(true);
            cfg.TimeBetweenEvictionRunsMillis = 30000;
            cfg.TestWhileIdle = true;
            cfg.JmxEnabled = true;
            pool = new PooledClientSource<DemoSoftQuery.Interface>("baidu91.appsvc.demo.DemoSoftQuery v1.0-CSharp", cfg, 10000);
            //pool = new ClientSource<DemoSoftQuery.Iface>("baidu91.appsvc.demo.DemoSoftQuery v1.0", 10000);
            TaskContext.instance().send_after(8000, getName(), new Message("request","ok"));
        }

        protected override void terminate(Exception ex)

        {
            if (pool != null)
            {
                pool.Dispose();
            }
        }

    }

}