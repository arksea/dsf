using System;
using System.Collections.Generic;

namespace com.baidu.dsf.server
{

    using Message = com.baidu.dsf.core.Message;
    using MessageChildTask = com.baidu.dsf.core.MessageChildTask;
    using TaskContext = com.baidu.dsf.core.TaskContext;

    /// <summary>
    /// 服务拨测，每30秒测试服务是否正常
    /// 
    /// @author arksea
    /// </summary>
    public class DialedTask : MessageChildTask //<IList<ThriftServiceManager>>
    {

        public static string TASK_NAME = "dialed_task";
        public static long DEFAULT_TIMEOUT = 10000;
        public static long DIALED_DELAY = 30000;
        public static long STAT_DELAY = 5000;

        public static IList<IServiceManager> createArgs()
        {
            return new List<IServiceManager>();
        }

        public DialedTask(string name, int msgQueuLen, IList<IServiceManager> args)
            : base(name, msgQueuLen, args)
        {
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected void handle_info(com.baidu.dsf.core.Message msg, String from) throws Throwable
        protected internal override void handle_info(Message msg, string from)
        {
            string key = msg.name;
            IList<IServiceManager> list = (IList<IServiceManager>)state;
            if (key.Equals("on_stat"))
            {
                try
                {
                    foreach (IServiceManager s in list)
                    {
                        s.onStat();
                    }
                }
                finally
                {
                    TaskContext.instance().send_after(STAT_DELAY, TASK_NAME, new Message("on_stat", ""));
                }
            }
            else if (key.Equals("on_dialed"))
            {
                try
                {
                    foreach (IServiceManager s in list)
                    {
                        s.onDialed();
                    }
                }
                finally
                {
                    TaskContext.instance().send_after(DIALED_DELAY, TASK_NAME, new Message("on_dialed", ""));
                }
            }
            else if (key.Equals("register"))
            {
                list.Add((IServiceManager)msg.value);
            }
            else if (key.Equals("unregister"))
            {
                list.Remove((IServiceManager)msg.value);
            }
        }

        protected internal override void init()
        {
            TaskContext.instance().send_after(10000, TASK_NAME, new Message("on_dialed", ""));
            TaskContext.instance().send_after(7000, TASK_NAME, new Message("on_stat", ""));
        }

        protected internal override void terminate(Exception ex)
        {
        }

        protected internal override Message handle_call(Message msg, string @string)
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

    }

}