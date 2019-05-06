namespace com.baidu.dsf.core
{

    /// <summary>
    /// 线程监控子任务类，继承自Thread，由Supervisor启动并注册其为UncaughtExceptionHandler，
    /// 当子任务线程异常退出时将会被Supervisor重启。
    /// $Author: xiaohaixing_298852 $
    /// </summary>
    public abstract class ChildTask : java.lang.Thread
    {

        protected internal readonly object state;
        /*
         * 构造函数增加子任务用于初始化的参数，
         * 当异常退出被重启时将由Supervisor重新创建并传入
         */

        public ChildTask(string name, object state) : base(name)
        {
            this.state = state;
        }

        public abstract void normalStopRequest();
    }

}