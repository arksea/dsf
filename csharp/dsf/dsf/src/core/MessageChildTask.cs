using System;

namespace com.baidu.dsf.core
{

    //using Logger = org.slf4j.Logger;
    //using LoggerFactory = org.slf4j.LoggerFactory;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public abstract class MessageChildTask : ChildTask
    {

        private enum MessageType
        {

            CALL,
            INFO
        }

        private class TypedMessage
        {
            private readonly MessageChildTask outerInstance;


            public TypedMessage(MessageChildTask outerInstance, MessageType type, Message message, string from)
            {
                this.outerInstance = outerInstance;
                this.type = type;
                this.message = message;
                this.from = from;
                this.result = null;
            }

            public TypedMessage(MessageChildTask outerInstance, MessageType type, Message message, string from, Result result)
            {
                this.outerInstance = outerInstance;
                this.type = type;
                this.message = message;
                this.from = from;
                this.result = result;
            }
            public readonly MessageType type;
            public readonly Message message;
            public readonly string from;
            public readonly Result result;
        }

        //private static readonly Logger logger = LoggerFactory.getLogger(typeof(Thread).FullName);
        //stopInfo: 退出请求信息，
        /*
         * 为了避免某些JVM在volatile上实现的缺陷，只使用一个volatile变量进行退出判断。
         * 当stopInfo等于null时认为没有退出请求；
         * 当stopInfo等于"normal"时认为是一个正常退出的请求；
         * 当stopInfo等于其他值时，认为是因为错误而收到的退出请求，stopInfo的值即为错误信息。
         */
        private volatile string stopInfo = null;
        private int maxQueueLen;
        private readonly java.util.concurrent.LinkedBlockingQueue queue = new java.util.concurrent.LinkedBlockingQueue();

        //声明Throwable异常意味着任务可以抛出任何自己无法处理的异常，交由框架统一处理：记录日志并重启任务
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: protected abstract void handle_info(Message msg, String from) throws Throwable;
        protected internal abstract void handle_info(Message msg, string from);

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: protected abstract Message handle_call(Message msg, String from) throws Throwable;
        protected internal abstract Message handle_call(Message msg, string from);

        public MessageChildTask(string name, int maxQueueLen, object args) : base(name, args)
        {
            this.maxQueueLen = maxQueueLen;
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private void tack(java.util.List<TypedMessage> msgList) throws InterruptedException
        private void tack(java.util.List msgList)
        {
            TypedMessage tmsg = (TypedMessage)queue.take();
            msgList.add(tmsg);
            queue.drainTo(msgList);
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private final void execute() throws Throwable
        private void execute()
        {
            java.util.LinkedList msgList = new java.util.LinkedList();
            while (!Stopped)
            {
                try
                {
                    tack(msgList);
                }
                catch (java.lang.InterruptedException ex)
                {
                    if (!Stopped)
                    {
                        //没有退出请求任务继续运行
                        java.lang.Thread.interrupted();
                        DSFLogHelper.WarnLog("MessageChildTask", ex , "wait message interrupted");
                    }
                    else
                    {
                        return;
                    }
                }
                foreach (TypedMessage m in msgList)
                {
                    execute(m);
                }
                msgList.clear();
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private final void execute(TypedMessage tmsg) throws Throwable
        private void execute(TypedMessage tmsg)
        {
            if (tmsg.type == MessageType.INFO)
            {
                handle_info(tmsg.message, tmsg.from);
            }
            else
            {
                if (tmsg.type == MessageType.CALL)
                {
                    try
                    {
                        tmsg.result.msg = handle_call(tmsg.message, tmsg.from);
                        tmsg.result.resulted = true;
                    }
                    catch (Exception ex)
                    {
                        //当被调用Task异常退出时，调用者也应抛出异常
                        tmsg.result.msg = new ThrowMessage(ex);
                        throw ex;
                    }
                    finally
                    {
                        lock (tmsg.result)
                        {
                            System.Threading.Monitor.Pulse(tmsg.result);
                        }
                    }
                }
            }
        }

        public void putInfo(Message msg)
        {
            if (msg == null) throw new ArgumentNullException();
            try
            {
                //此处长度判断与put不作同步，最多使队列长度达到maxQueueLen+并发线程数
                if (maxQueueLen > 0 && queue.size() >= maxQueueLen)
                {
                        throw new TaskQueueOverflowException("ChildTask " + getName() + "'s queue overflow");
                }
                TypedMessage tmsg = new TypedMessage(this, MessageType.INFO, msg, java.lang.Thread.currentThread().getName());
                queue.put(tmsg);
            }
            catch (java.lang.InterruptedException ex)
            {
                throw new TaskInterruptedException("send interrupted  ***  info=" + msg.name + ",sender=" + java.lang.Thread.currentThread().getName() + ",dest=" + getName(), ex);
            }
        }

        public void putCall(Message msg, Result result)
        {
            if (msg == null) throw new ArgumentNullException();
            try
            {
                //此处长度判断与put不作同步，最多使队列长度达到maxQueueLen+并发线程数
                if (maxQueueLen > 0 && queue.size() >= maxQueueLen)
                {
                    throw new TaskQueueOverflowException("ChildTask " + getName() + "'s queue overflow");
                }
                TypedMessage tmsg = new TypedMessage(this, MessageType.CALL, msg, java.lang.Thread.currentThread().getName(), result);
                queue.put(tmsg);
            }
            catch (java.lang.InterruptedException ex)
            {
                throw new TaskInterruptedException("call interrupted  ***  method=" + msg.name + ",caller=" + java.lang.Thread.currentThread().getName() + ",dest=" + getName(), ex);
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: protected final Message receive(IMessageMatcher matcher, long timeout) throws java.util.concurrent.TimeoutException, Throwable
        protected internal Message receive(IMessageMatcher matcher, long timeout)
        {
            if (matcher == null) throw new ArgumentNullException();
            //receive只应在本ChildTask线程中访问
            string n1 = getName();
            string n2 = java.lang.Thread.currentThread().getName();
            if (!n1.Equals(n2))
            {
                throw new ApplicationException("receive只应在本ChildTask线程中访问, current thread="+n2+", task thread="+n1);
            }
            java.util.List temp = new java.util.LinkedList();
            TypedMessage tmsg;
            while (!Stopped)
            {
                tmsg = (TypedMessage)queue.poll(timeout, java.util.concurrent.TimeUnit.MILLISECONDS);
                if (tmsg != null)
                {
                    if (tmsg.type == MessageType.INFO)
                    {
                        Message ret = matcher.match(tmsg.message);
                        if (ret != null)
                        {
                            lock (queue)
                            {
                                queue.drainTo(temp);
                                queue.addAll(temp);
                            }
                            temp.clear();
                            return ret;
                        }
                        else
                        {
                            temp.add(tmsg);
                        }
                    }
                    else
                    {
                        temp.add(tmsg);
                    }
                } //timeout
                else
                {
                    lock (queue)
                    {
                        queue.drainTo(temp);
                        queue.addAll(temp);
                    }
                    temp.clear();
                    throw new TimeoutException();
                }
            }
            lock (queue)
            {
                queue.drainTo(temp);
                queue.addAll(temp);
            }
            temp.clear();
            throw new java.lang.InterruptedException();
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: protected final Message receive(IMessageMatcher matcher) throws java.util.concurrent.TimeoutException, Throwable
        protected internal Message receive(IMessageMatcher matcher)
        {
            if (matcher == null) throw new ArgumentNullException();
            //receive只应在本ChildTask线程中访问
            string n1 = getName();
            string n2 = java.lang.Thread.currentThread().getName();
            if (!n1.Equals(n2))
            {
                throw new ApplicationException("receive只应在本ChildTask线程中访问, current thread=" + n2 + ", task thread=" + n1);
            }
            java.util.List temp = new java.util.LinkedList();
            TypedMessage tmsg;
            while (!Stopped)
            {
                tmsg = (TypedMessage)queue.take();
                if (tmsg.type == MessageType.INFO)
                {
                    Message ret = matcher.match(tmsg.message);
                    if (ret != null)
                    {
                        lock (queue)
                        {
                            queue.drainTo(temp);
                            queue.addAll(temp);
                        }
                        temp.clear();
                        return ret;
                    }
                    else
                    {
                        temp.add(tmsg);
                    }
                }
                else
                {
                    temp.add(tmsg);
                }
            }
            lock (queue)
            {
                queue.drainTo(temp);
                queue.addAll(temp);
            }
            temp.clear();
            throw new java.lang.InterruptedException();
        }

        public override void run()
        {
            DSFLogHelper.DebugLog("MessageChildTask", "ChildTask '" + getName() + "' start up");
            Exception runtimeEx = null;
            try
            {
                this.init();
                this.execute();
            }
            catch (Exception ex)
            {
                runtimeEx = ex;
            }

            DSFLogHelper.DebugLog("MessageChildTask", "ChildTask '" + getName() + "' stopped");
            if (stopInfo == null) //没有退出请求，根据是否有异常进行处理
            {
                terminate(runtimeEx);
                if (runtimeEx == null)
                {
                    stopInfo = "normal";
                }
                else
                {
                    stopInfo = runtimeEx.Message;
                    if (stopInfo == null)
                    {
                        stopInfo = runtimeEx.GetType().Name;
                    }
                    throw new Exception("terminated",runtimeEx);
                }
            }
            else
            {
                if (stopInfo.Equals("normal")) //收到正常退出请求，不抛出异常
                {
                    terminate(null);
                } //收到错误退出请求
                else
                {
                    if (runtimeEx == null)
                    {
                        runtimeEx = new Exception(stopInfo);
                        terminate(runtimeEx);
                        throw runtimeEx;
                    }
                    else
                    {
                        terminate(runtimeEx);
                        throw new Exception("terminated", runtimeEx);
                    }
                }
            }
        }

        public void errorStopRequest(string error)
        {
            stopInfo = error;
            this.interrupt();
        }

        public bool Stopped
        {
            get
            {
                return stopInfo != null;
            }
        }

        public bool ErrorStopped
        {
            get
            {
                return stopInfo != null && !stopInfo.Equals("normal");
            }
        }

        public bool NormalStopped
        {
            get
            {
                return stopInfo != null && stopInfo.Equals("normal");
            }
        }

        public string StopInfo
        {
            get
            {
                return stopInfo;
            }
        }

        public override sealed void normalStopRequest()
        {
            stopInfo = "normal";
            this.interrupt();
        }

        public virtual long MaxQueueLen
        {
            get
            {
                return maxQueueLen;
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: protected abstract void init() throws Throwable;
        protected internal abstract void init();

        protected internal abstract void terminate(Exception ex);
    }

}