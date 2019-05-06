package dsf.core;

/**
 * 线程监控子任务类，继承自Thread，由Supervisor启动并注册其为UncaughtExceptionHandler，
 * 当子任务线程异常退出时将会被Supervisor重启。
 * $Author: xiaohaixing_298852 $
 */
public abstract class ChildTask<T> extends Thread {

    protected final T state;
    /*
     * 构造函数增加子任务用于初始化的参数，
     * 当异常退出被重启时将由Supervisor重新创建并传入
     */

    public ChildTask(String name, T state) {
        super(name);
        this.state = state;
    }

    public abstract void normalStopRequest();
}
