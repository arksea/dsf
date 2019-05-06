using System;
//using System.Threading;

namespace com.baidu.dsf.core
{


    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class TaskContext
    {

        private static readonly TaskContext context = new TaskContext();
        private readonly java.util.Map serviceMap = new java.util.concurrent.ConcurrentHashMap();
        private readonly java.util.List supList = new java.util.LinkedList();
        private readonly java.util.Timer timer = new java.util.Timer("TaskContext Timer", true);

        public static TaskContext instance()
        {
            return context;
        }

        public virtual Supervisor start(string supervisorName, RestartStrategies restart, ChildInfo[] childs)
        {
            lock (this)
            {
                Supervisor sup = new Supervisor(supervisorName, restart, childs);
                supList.add(sup);
                return sup;
            }
        }

        public virtual Supervisor start(RestartStrategies restart, ChildInfo child)
        {
            lock (this)
            {
                Supervisor sup = new Supervisor(child.name, restart, new ChildInfo[] { child });
                supList.add(sup);
                return sup;
            }
        }

        public virtual void stopAll()
        {
            lock (this)
            {
                for (int i = supList.size()-1; i>=0; --i)
                {
                    Supervisor s = (Supervisor)supList.get(i);
                    s.stopAll();
                }
                supList.clear();
            }
        }

        public virtual void stop(string supervisorName)
        {
            lock (this)
            {
                java.util.Iterator it = supList.iterator();
                while (it.hasNext())
                {
                    Supervisor s = (Supervisor)it.next();
                    if (s.name.Equals(supervisorName))
                    {
                        s.stopAll();
                        supList.remove(s);
                        break;
                    }
                }
            }
        }

        private TaskContext()
        {
        }

        protected internal virtual void put(string name, MessageChildTask svc)
        {
            //不能直接替换子任务，否则被替换者将会成为一个"悬挂"的同名子任务
            if (serviceMap.get(name) != null)
            {
                throw new Exception("register a ChildTask failed,The name '" + name + "' is exist");
            }
            serviceMap.put(name, svc);
        }

        protected internal virtual void remove(string name)
        {
            serviceMap.remove(name);
        }

        protected internal virtual MessageChildTask get(string name)
        {
            return (MessageChildTask)serviceMap.get(name);
        }

        public virtual bool exist(string name)
        {
            return serviceMap.containsKey(name);
        }

        //发送消息给目标任务，这种调用不会阻塞调用者，也不会阻塞被调用者
        public virtual void send(string dest, Message msg)
        {
            MessageChildTask svc = get(dest);
            if (svc != null)
            {
                svc.putInfo(msg);
            }
            else
            {
                throw new TaskNotExistException(dest);
            }
        }
    // 直接向Task引用发送消息，会在Task被重启后发给一个已经死掉的Task
    //    public void send(MessageChildTask svc, Message msg) {
    //        if (svc != null) {
    //            svc.putInfo(msg);
    //        }
    //    }
        //延迟发送消息给目标任务，这种调用不会阻塞调用者，也不会阻塞被调用者，
        //注意：对于向自身投递消息的Task，如果延时期间失败被重启，消息不会被送到新的任务线程中
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: public void send_after(final long milliseconds, final String dest, final Message msg)
        public virtual void send_after(long milliseconds, string dest, Message msg)
        {
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final MessageChildTask svc = get(dest);
            MessageChildTask svc = get(dest);
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final String source = Thread.currentThread().getName();
            string source = java.lang.Thread.currentThread().getName();
            if (svc != null)
            {
                java.util.TimerTask task = new TimerTaskAnonymousInnerClassHelper(this, dest, msg, svc, source);
                timer.schedule(task, milliseconds);
            }
            else
            {
                throw new TaskNotExistException(dest);
            }
        }

        private class TimerTaskAnonymousInnerClassHelper : java.util.TimerTask
        {
            private readonly TaskContext outerInstance;

            private string dest;
            private com.baidu.dsf.core.Message msg;
            private com.baidu.dsf.core.MessageChildTask svc;
            private string source;

            public TimerTaskAnonymousInnerClassHelper(TaskContext outerInstance, string dest, com.baidu.dsf.core.Message msg, com.baidu.dsf.core.MessageChildTask svc, string source)
            {
                this.outerInstance = outerInstance;
                this.dest = dest;
                this.msg = msg;
                this.svc = svc;
                this.source = source;
            }


            public override void run()
            {
                if (source.Equals(dest))
                {
                    //向自己投递延时消息将直接使用消息发送时取得的svc实例发送消息，
                    //这样，如果延时期间失败被重启，消息不会被送到新的任务线程中
                    //对比重新根据任务名取得新的实例应用svcnew = get(dest)，这种选择副作用较小,
                    svc.putInfo(msg);
                }
                else
                {
                    //对于外部线程发送的消息，将尽量投递到新的有效的任务线程中
                    MessageChildTask newsvc = outerInstance.get(dest);
                    if (newsvc != null)
                    {
                        newsvc.putInfo(msg);
                    }
                }

            }
        }
    //    public void send_after(final long milliseconds, final MessageChildTask svc, final Message msg) {
    //        assert (svc!=null);
    //        TimerTask task = new TimerTask() {
    //            @Override
    //            public void run() {
    //                svc.putInfo(msg);
    //            }
    //        };
    //        timer.schedule(task, milliseconds);
    //    }
        //发送消息给目标任务，要求其处理请求后返回处理结果，这种调用会阻塞调用者，也会阻塞被调用者
        public virtual Message call(string dest, Message callmsg, long timeout)
        {
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final MessageChildTask svc = get(dest);
            MessageChildTask svc = get(dest);
            if (svc == null)
            {
                throw new TaskNotExistException("task not exist  ***  method=" + callmsg.name + ",caller=" + java.lang.Thread.currentThread().getName() + ",dest=" + dest);
            }
            return call(svc, callmsg, timeout);
        }
        private Message call(MessageChildTask svc, Message callmsg, long timeout)
        {
            //不得从任务线程本身call自己，防止死锁
            string n1 = svc.getName();
            string n2 = java.lang.Thread.currentThread().getName();
            if (svc==null || n1.Equals(n2))
            {
                throw new ApplicationException("不得从任务线程本身call自己，防止死锁, current thread=" + n2 + ", task thread=" + n1);
            }
            string dest = svc.getName();
            if (timeout == 0)
            {
                throw new Exception("timeout must not be zero.");
            }
            //每次都创建新的Result实例，并在其上wait/notify，防止错误的信号通知
            Result result = new Result();
            lock (result)
            {
                svc.putCall(callmsg, result);
                try
                {
                    System.Threading.Monitor.Wait(result, TimeSpan.FromMilliseconds(timeout));
                }
                catch (System.Threading.ThreadInterruptedException ex)
                {
                    throw new TaskInterruptedException("call interrupted  ***  method=" + callmsg.name + ",caller=" + java.lang.Thread.currentThread().getName() + ",dest=" + dest, ex);
                }
            }
            if (result.msg != null && (result.msg is ThrowMessage))
            {
                object v = result.msg.value;
                if (v is Exception)
                {
                    throw new TaskCallException("  ***  method=" + callmsg.name + ",caller=" + java.lang.Thread.currentThread().getName() + ",dest=" + dest, (Exception) v);
                }
                else
                {
                    throw new TaskCallException((string) v + "  ***  method=" + callmsg.name + ",caller=" + java.lang.Thread.currentThread().getName() + ",dest=" + dest);
                }
            }
            else
            {
                if (!result.resulted)
                {
                    throw new TaskCallException("call timeout  ***  method=" + callmsg.name + ",caller=" + java.lang.Thread.currentThread().getName() + ",dest=" + dest);
                }
                else
                {
                    return result.msg;
                }
            }
        }
    }

}