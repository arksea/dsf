using System;

namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea </summary>
    /// @param <T> 线程任务状态类 </param>
    public class ChildInfo
    {

        public readonly string name;
        public readonly java.lang.Class clazz;
        //最大消息队列长度，当设置为0时，将不检测队列长度
        //但这很危险，请谨慎抉择
        public readonly int maxQueueLen;
        public readonly object state;
        private Type stateClass;
        private readonly bool daemon;

        public ChildInfo(string name, java.lang.Class clz, object state)
        {
            this.name = name;
            this.clazz = clz;
            this.state = state;
            this.maxQueueLen = 10;
            this.daemon = false;
            this.stateClass = state.GetType();
        }

        public ChildInfo(string name, java.lang.Class clz, object state, int maxQueueLen)
        {
            this.name = name;
            this.clazz = clz;
            this.state = state;
            this.maxQueueLen = maxQueueLen;
            this.daemon = false;
            this.stateClass = state.GetType();
        }

        public ChildInfo(string name, java.lang.Class clz, object state, int maxQueueLen, bool daemon)
        {
            this.name = name;
            this.clazz = clz;
            this.state = state;
            this.maxQueueLen = maxQueueLen;
            this.daemon = daemon;
            this.stateClass = state.GetType();
        }

        public virtual bool Daemon
        {
            get
            {
                return daemon;
            }
        }

        public virtual Type ArgsClass
        {
            get
            {
                return stateClass;
            }
            set
            {
                this.stateClass = value;
            }
        }

    }

}