using System;

namespace com.baidu.dsf.server
{

    using ChildTask = com.baidu.dsf.core.ChildTask;
    using Thrift.Protocol;
    using Thrift.Server;
    using Thrift.Transport;
    using com.baidu.dsf.core;
    using java.util.concurrent.atomic;

    /// <summary>
    /// 应用服务端运行线程，用于将应用服务纳入监控树中，
    /// 当应用服务异常退出时将被Supervisor重启
    /// @author arksea </summary>
    /// @param <T> </param>
    public class ThriftServiceTask : ChildTask 
    {
        private volatile bool normalStop = false;
        private TServer server;
        private readonly AtomicInteger connectionCount = new AtomicInteger(0);
        private readonly AtomicLong requestCount = new AtomicLong(0L);

        public ThriftServiceTask(string name, IThriftServerFactory args)
            : base(name, args)
        {
        }

        public override void run()
        {
            try
            {
                lock (this)
                {
                    server = (state as IThriftServerFactory).create();
                }
                DSFLogHelper.DebugLog("ThriftServcieTask", "service task " + getName() + " is started");
                if (!normalStop)
                {
                    server.Serve();
                }
                if (normalStop)
                {
                    DSFLogHelper.DebugLog("ThriftServcieTask", "service task " + getName() + " is stopped");
                }
                else
                {
                    throw new Exception("service task " + getName() + " failed");
                }
            }
            catch (Exception ex)
            {
                throw new Exception("service task " + getName() + " failed", ex);
            }
        }

        public override void normalStopRequest()
        {
            lock (this)
            {
                normalStop = true;
                if (server != null)
                {
                    server.Stop();
                }
            }
        }

        public virtual int ConnectionCount
        {
            get
            {
                return connectionCount.get();
            }
        }

        public virtual long RequestCount
        {
            get
            {
                return requestCount.get();
            }
        }
    }

}