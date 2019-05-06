using System;
using System.Collections.Generic;

namespace com.baidu.dsf.register
{
    using com.baidu.dsf.core;
    using java.nio;
    using java.nio.channels;
    using java.net;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class RegisterClient : MessageChildTask
    {

        public static string TASK_NAME = "regclient";
        public static string LOGIN_NAME = "anonymous";
        public static long DEFAULT_TIMEOUT = 10000;
        private int missAckTickCount = 0;
        private long tickid = 0;
        private AsynchronousSocketChannel channel;
        private SocketWritter writter;
        private CompletionReader reader;
        private static TaskContext context = TaskContext.instance();
        private readonly IMessageMatcher succeedMatcher = new NamedMsgsMatcher(MSG_NAMES.MSG_TCP, new string[] { MSG_NAMES.MSG_SUCCEED, MSG_NAMES.MSG_ERROR_RESULT });

        public static ChildInfo createChildInfo(string loginName, InetSocketAddress[] addrs, int maxQueueSize)
        {
            RegisterClient.LOGIN_NAME = loginName;
            TaskState s = new TaskState(addrs);
            return new ChildInfo(RegisterClient.TASK_NAME, typeof(RegisterClient), s, maxQueueSize);
        }

        public class TaskState
        {

            internal readonly InetSocketAddress[] addrs;
            internal string subid = "0";
            internal long retryDelay = 0; // 重连延迟时间,首次连接不延迟
            internal int sockIdx = 0;

            public TaskState(InetSocketAddress[] addrs)
            {
                this.addrs = addrs;
            }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public synchronized void delay() throws InterruptedException
            public virtual void delay()
            {
                lock (this)
                {
                    if (retryDelay > 0)
                    {
                        sleep(retryDelay);
                        retryDelay *= 2;
                        if (retryDelay > 30000)
                        {
                            retryDelay = 30000;
                        }
                    } // 首次连接不延迟
                    else
                    {
                        retryDelay = 2000;
                    }
                }
            }

            public virtual void resetDelay()
            {
                lock (this)
                {
                    retryDelay = 2000;
                }
            }
        }

        // ---------------------------------------------------------------------------
        // 以下系列方法为 RegisterClient 导出的API，
        // 这些静态方法仅是为了简化TaskContext.call()的使用,
        // 在MessageChildTask中将会频繁看到这种范式
        public static void subscribeService(string regname)
        {
            context.call(TASK_NAME, new Message(MSG_NAMES.MSG_SUBSCRIBE, regname), DEFAULT_TIMEOUT);
        }

        public static void registerService(MsgServiceReg svc)
        {
            context.call(TASK_NAME, new Message(MSG_NAMES.MSG_REGSVC, svc), DEFAULT_TIMEOUT);
        }

        public static void unregisterService(MsgServiceReg svc)
        {
            context.call(TASK_NAME, new Message(MSG_NAMES.MSG_UNREGSVC, svc), DEFAULT_TIMEOUT);
        }

        public static void reportServiceState(MsgServiceReg svc, string name, string value)
        {
            MsgReportSvcState obj = new MsgReportSvcState(svc, name, value);
            context.send(TASK_NAME, new Message(MSG_NAMES.MSG_REPORT_SVC_STATE, obj));
        }

        public static void setServiceState(MsgServiceReg svc, IList<MsgSvcState> states)
        {
            context.call(TASK_NAME, new Message(MSG_NAMES.MSG_SET_SVC_STATE, new MsgNotifySvcState(svc.regname, new MsgSvcAddr(svc.host, svc.port), states)), DEFAULT_TIMEOUT);
        }

        public static MsgServiceDefine queryServiceDefine(string regname)
        {
            Message ret = context.call(TASK_NAME, new Message(MSG_NAMES.MSG_QUERY_SVCDEF, regname), DEFAULT_TIMEOUT);
            MsgServiceDefine def = (MsgServiceDefine) ret.value;
            return def;
        }

        public static MsgSubscribeResult queryServiceState(string regname)
        {
            Message ret = context.call(TASK_NAME, new Message(MSG_NAMES.MSG_QUERY_SVC_STATE, regname), DEFAULT_TIMEOUT);
            MsgSubscribeResult sub = (MsgSubscribeResult) ret.value;
            return sub;
        }

        private class SvcdefIDL
        {
            internal MsgServiceDefine def;
            internal string idl;
        }
        public static void addServiceDefine(MsgServiceDefine def, string idl)
        {
            SvcdefIDL v = new SvcdefIDL();
            v.def = def;
            v.idl = idl;
            Message param = new Message(MSG_NAMES.MSG_ADD_SVCDEF, v);
            context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
        }

        public static void updateServiceDefine(MsgServiceDefine def)
        {
            Message param = new Message(MSG_NAMES.MSG_UPDATE_SVCDEF, def);
            context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
        }

        public static void updateServiceDefine(MsgServiceDefine def, string idl)
        {
            SvcdefIDL v = new SvcdefIDL();
            v.def = def;
            v.idl = idl;
            Message param = new Message(MSG_NAMES.MSG_UPDATE_SVCDEF_IDL, v);
            context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
        }

        public static void deleteServiceDefine(string regname)
        {
            Message param = new Message(MSG_NAMES.MSG_DEL_SVCDEF, regname);
            context.call(TASK_NAME, param, DEFAULT_TIMEOUT);
        }

        // ---------------------------------------------------------------------------
        public RegisterClient(string name, int maxQueueLen, TaskState state) : base(name, maxQueueLen, state)
        {
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected void init() throws Throwable
        protected internal override void init()
        {
            context = TaskContext.instance();
            ((TaskState)state).delay();

            initSocketChannel();
            writter = new SocketWritter(channel);
            login();
            context.send_after(1000, getName(), new Message(MSG_NAMES.MSG_TICK, ""));
            // 正常运行1分钟后重置重试时间，
            // 不直接调用resetDelay是为了防止登录后短时间内出现异常造成的频繁重连
            context.send_after(60000, getName(), new Message("reset_delay", ""));
        }

        protected internal override void terminate(Exception ex)
        {
            if (channel != null)
            {
                try
                {
                    channel.close();
                }
                catch (java.io.IOException ex1)
                {
                    DSFLogHelper.WarnLog("RegisterClient", ex1, "close socket channel error");
                }
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected void handle_info(com.baidu.dsf.core.Message msg, String from) throws Throwable
        protected internal override void handle_info(Message msg, string from)
        {
            string key = msg.name;
            if (key.Equals(MSG_NAMES.MSG_TICK))
            {
                if (++missAckTickCount > 3)
                {
                    errorStopRequest("miss tick ack count > 3");
                }
                else
                {
                    writter.send(MSG_NAMES.MSG_TICK, tickid);
                    tickid++;
                    context.send_after(30000, this.getName(), msg);
                }
            }
            else if (key.Equals(MSG_NAMES.MSG_TCP))
            {
                Message tcpmsg = (Message) msg.value;
                if (tcpmsg.name.Equals(MSG_NAMES.MSG_TACK))
                {
                    missAckTickCount = 0;
                }
                else
                {
                    handle_tcp_info(tcpmsg.name, tcpmsg.value, from);
                }
            }
            else if (key.Equals(MSG_NAMES.MSG_REPORT_SVC_STATE))
            {
                writter.send(key, msg.value);
            }
            else if (key.Equals("channel_error"))
            {
                if (!Stopped)
                {
                    errorStopRequest((string) msg.value);
                }
            }
            else if (key.Equals("reset_delay"))
            {
                ((TaskState)state).resetDelay();
            }
            else
            {
                DSFLogHelper.ErrorLog("RegisterClient", "unknown message : " + msg);
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override protected com.baidu.dsf.core.Message handle_call(com.baidu.dsf.core.Message msg, String from) throws Throwable
        protected internal override Message handle_call(Message msg, string from)
        {
            string key = msg.name;
            if (key.Equals(MSG_NAMES.MSG_SUBSCRIBE)) // 订阅服务
            {
                string regname = (string) msg.value;
                if (ServiceManager.isSubscribed(regname))
                {
                    return new Message("ok", "");
                }
                writter.send(msg);
                IMessageMatcher m = new NamedMsgsMatcher(MSG_NAMES.MSG_TCP, new string[] { MSG_NAMES.MSG_SUBSCRIBE_RESULT, MSG_NAMES.MSG_ERROR_RESULT });
                Message ret = this.receive(m, DEFAULT_TIMEOUT);
                if (ret.name.Equals(MSG_NAMES.MSG_SUBSCRIBE_RESULT))
                {
                    MsgSubscribeResult sub = (MsgSubscribeResult) ret.value;
                    ServiceManager.addSubscribe(sub);
                    return new Message("ok", "");
                }
                else
                {
                    return new ThrowMessage(ret.value.ToString());
                }
            }
            else if (key.Equals(MSG_NAMES.MSG_QUERY_SVC_STATE))
            {
                writter.send(msg);
                IMessageMatcher m = new NamedMsgsMatcher(MSG_NAMES.MSG_TCP, new string[] { MSG_NAMES.MSG_QUERY_SVC_STATE_RESULT, MSG_NAMES.MSG_ERROR_RESULT });
                Message ret = this.receive(m, DEFAULT_TIMEOUT);
                if (ret.name.Equals(MSG_NAMES.MSG_QUERY_SVC_STATE_RESULT))
                {
                    MsgSubscribeResult sub = (MsgSubscribeResult) ret.value;
                    return new Message("ok", sub);
                }
                else
                {
                    return new ThrowMessage(ret.value.ToString());
                }
            } // 注册服务
            else if (key.Equals(MSG_NAMES.MSG_REGSVC))
            {
                writter.send(key, msg.value);
                return waitForSucceedMsg();
            }
            else if (key.Equals(MSG_NAMES.MSG_UNREGSVC))
            {
                writter.send(key, msg.value);
                return waitForSucceedMsg();
            } 
            else if (key.Equals(MSG_NAMES.MSG_SET_SVC_STATE))
            {
                writter.send(key, msg.value);
                return waitForSucceedMsg();
            }
            // 查询服务定义
            else if (key.Equals(MSG_NAMES.MSG_QUERY_SVCDEF))
            {
                writter.send(msg);
                IMessageMatcher m = new NamedMsgsMatcher(MSG_NAMES.MSG_TCP, new string[] { MSG_NAMES.MSG_QUERY_SVCDEF_RESULT, MSG_NAMES.MSG_ERROR_RESULT });
                Message ret = this.receive(m, DEFAULT_TIMEOUT);
                if (ret.name.Equals(MSG_NAMES.MSG_QUERY_SVCDEF_RESULT))
                {
                    MsgServiceDefine def = (MsgServiceDefine) ret.value;
                    return new Message("ok", def);
                }
                else
                {
                    return new ThrowMessage(ret.value.ToString());
                }
            } // 添加服务定义
            else if (key.Equals(MSG_NAMES.MSG_ADD_SVCDEF))
            {
                SvcdefIDL v = (SvcdefIDL)msg.value;
                object[] args = new object[] {v.def, v.idl};
                writter.send(key, args);
                return waitForSucceedMsg();
            } // 修改服务定义
            else if (key.Equals(MSG_NAMES.MSG_UPDATE_SVCDEF))
            {
                writter.send(key, msg.value);
                return waitForSucceedMsg();
            } // 修改服务定义
            else if (key.Equals(MSG_NAMES.MSG_UPDATE_SVCDEF_IDL))
            {
                SvcdefIDL v = (SvcdefIDL)msg.value;
                object[] args = new object[] {v.def, v.idl};
                writter.send(key, args);
                return waitForSucceedMsg();
            } // 删除服务定义
            else if (key.Equals(MSG_NAMES.MSG_DEL_SVCDEF))
            {
                writter.send(key, msg.value);
                return waitForSucceedMsg();
            }
            else
            {
                return null;
            }
        }

        private void handle_tcp_info(string name, object obj, string from)
        {
            if (name.Equals(MSG_NAMES.MSG_NOTIFY_SVC_STATE))
            {
                MsgNotifySvcState msg = (MsgNotifySvcState) obj;
                ServiceManager.notifyServiceState(msg);
            }
            else if (name.Equals(MSG_NAMES.MSG_NOTIFY_SVCDEF_UPDATE))
            {
                MsgServiceDefine msg = (MsgServiceDefine) obj;
                ServiceManager.notifySvcdefUpdate(msg);
            }
            else if (name.Equals(MSG_NAMES.MSG_ERROR))
            {
                DSFLogHelper.ErrorLog("RegisterClient", MSG_NAMES.MSG_ERROR + ": " + obj);
            }
            else
            {
                DSFLogHelper.WarnLog("RegisterClient", "not handle tcp info: " + name);
            }
        }

        // 等待succeed消息，所有返回succeed消息的tcp call都可以使用
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private com.baidu.dsf.core.Message waitForSucceedMsg() throws Throwable
        private Message waitForSucceedMsg()
        {
            Message ret = this.receive(succeedMatcher, DEFAULT_TIMEOUT);
            if (ret.name.Equals(MSG_NAMES.MSG_SUCCEED))
            {
                string s = (string) ret.value;
                return new Message("succeed", s);
            }
            else
            {
                return new ThrowMessage(ret.value.ToString());
            }
        }

        /// <summary>
        /// 获取注册服务器地址
        /// </summary>
        /// <param name="addrArray">
        /// @return </param>
        private InetSocketAddress getSockAddr(InetSocketAddress[] addrArray)
        {
            TaskState s = (TaskState)state;
            if (s.sockIdx >= addrArray.Length)
            {
                s.sockIdx = 0;
            }
            InetSocketAddress sockAddr = addrArray[s.sockIdx];
            s.sockIdx++;
            return sockAddr;
        }

        private void initSocketChannel()
        {
            InetSocketAddress addr = getSockAddr(((TaskState)state).addrs);
            try
            {
                // 轮流使用其中一个地址连接注册服务器
                channel = AsynchronousSocketChannel.open();
                channel.connect(addr).get();
                reader = new CompletionReader(channel, this);
            }
            catch (Exception ex)
            {
                throw new Exception("连接注册服务器失败," + addr.getHostString() + ":" + addr.getPort(), ex);
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private void login() throws Throwable
        private void login()
        {
            // 登录注册服务器
            IDictionary<string, string> json = new Dictionary<string, string>(3);
            json["subid"] = ((TaskState)state).subid;
            json["name"] = RegisterClient.LOGIN_NAME;
            json["version"] = "1.1";

            writter.send(MSG_NAMES.MSG_LOGIN, json);
            IMessageMatcher m = new NamedMsgsMatcher(MSG_NAMES.MSG_TCP, new string[] { MSG_NAMES.MSG_LOGIN_SUCCEED, MSG_NAMES.MSG_SYNC_SUBLIST, MSG_NAMES.MSG_ERROR_RESULT });
            while (true)
            {
                Message ret = this.receive(m, DEFAULT_TIMEOUT);
                if (ret.name.Equals(MSG_NAMES.MSG_LOGIN_SUCCEED))
                {
                    string newid = ret.value.ToString();
                    ((TaskState)state).subid = newid;
                    break;
                }
                else if (ret.name.Equals(MSG_NAMES.MSG_SYNC_SUBLIST))
                {
                    HashSet<string> subed = ServiceManager.Subscribed;
                    writter.send(MSG_NAMES.MSG_UPDATE_SUBLIST, subed);
                }
                else
                {
                    throw new DSFrameworkException(ret.ToString());
                }
            }
        }
    }

}