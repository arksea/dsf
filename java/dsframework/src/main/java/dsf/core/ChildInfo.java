package dsf.core;

/**
 *
 * @author arksea
 * @param <T> 线程任务状态类
 */
public class ChildInfo<T> {

    public final String name;
    public final Class clazz;
    //最大消息队列长度，当设置为0时，将不检测队列长度
    //但这很危险，请谨慎抉择
    public final long maxQueueLen;
    public final T state;
    private Class stateClass;
    private final boolean daemon;

    public ChildInfo(String name, Class clz, T state) {
        this.name = name;
        this.clazz = clz;
        this.state = state;
        this.maxQueueLen = 10;
        this.daemon = false;
        this.stateClass = state.getClass();
    }

    public ChildInfo(String name, Class clz, T state, long maxQueueLen) {
        this.name = name;
        this.clazz = clz;
        this.state = state;
        this.maxQueueLen = maxQueueLen;
        this.daemon = false;
        this.stateClass = state.getClass();
    }

    public ChildInfo(String name, Class clz, T state, long maxQueueLen, boolean daemon) {
        this.name = name;
        this.clazz = clz;
        this.state = state;
        this.maxQueueLen = maxQueueLen;
        this.daemon = daemon;
        this.stateClass = state.getClass();
    }

    public boolean isDaemon() {
        return daemon;
    }

    public Class getArgsClass() {
        return stateClass;
    }

    public void setArgsClass(Class argsClass) {
        this.stateClass = argsClass;
    }
}
