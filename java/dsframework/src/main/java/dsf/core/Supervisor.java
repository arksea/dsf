package dsf.core;

import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import com.google.common.collect.ImmutableList;

/**
 * 线程监控者，当线程ChildTask异常退出时负责将其重启
 * $Author: xiaohaixing_298852 $
 */
public class Supervisor implements Thread.UncaughtExceptionHandler {

    public final String name;
    
    private final static Logger logger = LogManager.getLogger(Supervisor.class.getName());
    private final RestartStrategies restartStrategies;
    private final Map<String, ChildTask> childMap = new HashMap();
    private final ImmutableList<ChildInfo> childList; //保留子任务启动顺序
    private final long JOIN_CHILD_TIMEOUT = 5000;
    private long restartCount = 0;
    private long lastRestartTime = 0;

    public ChildTask getChild(String childName) {
        return childMap.get(childName);
    }
    
    protected Supervisor(String name, RestartStrategies restart, ChildInfo[] childs) {
        synchronized(this) {
            logger.trace("Supervisor '" + name + "' starting childs");
            this.name = name;
            this.restartStrategies = restart;
            childList = ImmutableList.copyOf(childs);
            startAll();
            logger.trace("Supervisor '" + name + "' started all childs");
        }
    }

    protected synchronized void stopAll() {
        logger.trace("Supervisor '" + name + "' stopping childs");
        for (int i = childList.size()-1; i>=0; --i) {
            ChildInfo info = childList.get(i);
            ChildTask t = childMap.get(info.name);
            t.normalStopRequest();
            TaskContext.instance().remove(info.name);
            try {
                t.join(JOIN_CHILD_TIMEOUT);
            } catch (InterruptedException ex) {
            }
            if (t.isAlive()) {
                t.stop();//超时则强制退出线程
            }
        }
        logger.trace("Supervisor '" + name + "' stopped all childs");
    }

    private void startAll() {
        for (ChildInfo c : childList) {
            createChild(c);
        }
        for (ChildInfo c : childList) {
            Thread child = childMap.get(c.name);
            child.start();
        }
    }

    private Thread createChild(ChildInfo info) {
        try {
            ChildTask child;
            if (MessageChildTask.class.isAssignableFrom(info.clazz)) {
                Constructor con = info.clazz.getConstructor(String.class, long.class, info.getArgsClass());
                child = (ChildTask) con.newInstance(info.name, info.maxQueueLen, info.state);
                child.setDaemon(info.isDaemon());
                TaskContext.instance().put(info.name, (MessageChildTask) child);
            } else {
                Constructor con = info.clazz.getConstructor(String.class, info.getArgsClass());
                child = (ChildTask) con.newInstance(info.name, info.state);
                child.setDaemon(info.isDaemon());
            }
            child.setUncaughtExceptionHandler(this);
            childMap.put(info.name, child);
            return child;
        } catch (Throwable ex) {
            throw new RuntimeException("Supervisor '" + name + "' create ChildTask '" + info.name + "' failed", ex);
        }
    }

    @Override
    public void uncaughtException(Thread t, Throwable ex) {
        String childName = t.getName();
        logger.error("ChildTask '" + childName + "' exited because Exception", ex);
        ++restartCount;
        long now = System.currentTimeMillis();
        long time = now - lastRestartTime;
        lastRestartTime = now;
        if (time < 10000) {
            if (restartCount > 10) {
                logger.error("ChileTask异常重启次数短时间内超过10次，将被停止："+name);
                TaskContext.instance().stop(name);
            } else {
                restart(childName);
            }
        } else {
            //上次重启在10秒前则重启次数从头计算
            restartCount = 1;
            restart(childName);
        }
    }

    private void restart(String childName) {
        switch (restartStrategies) {
            case ONE_FOR_ONE:
                stopOneForOne(childName);
                break;
            case ONE_FOR_ALL:
                stopOneForAll(childName);
                break;
            case REST_FOR_ONE:
                stopRestForOne(childName);
                break;
        }
    }
    private void stopOneForOne(String childName) {
        for (ChildInfo i : childList) {
            if (i.name.equals(childName)) {
                TaskContext.instance().remove(childName);
                Thread child = createChild(i);
                child.start();
                break;
            }
        }
    }

    private void stopOneForAll(String childName) {
        for (ChildInfo i : childList) {
            TaskContext.instance().remove(childName);
            ChildTask child = childMap.get(i.name);
            child.normalStopRequest();
            try {
                child.join(JOIN_CHILD_TIMEOUT);
            } catch (InterruptedException ex) {
                logger.warn("Supervisor '" + name + "' join ChildTask '" + childName + "' interruped", ex);
            }
            if (child.isAlive()) {
                child.stop();//超时则强制退出线程
                logger.warn("Supervisor '" + name + "' join ChildTask '" + childName + "' timeout");
            }
        }
        for (ChildInfo i : childList) {
            Thread child = createChild(i);
            child.start();
        }
    }

    private void stopRestForOne(String childName) {
        boolean rest = false;
        for (ChildInfo i : childList) {
            if (rest || i.name.equals(childName)) {
                if (rest) {
                    TaskContext.instance().remove(childName);
                    ChildTask child = childMap.get(i.name);
                    child.normalStopRequest();
                    try {
                        child.join(JOIN_CHILD_TIMEOUT);
                    } catch (InterruptedException ex) {
                        logger.warn("Supervisor '" + name + "' join ChildTask '" + childName + "' interruped", ex);
                    }
                    if (child.isAlive()) {
                        child.stop();//超时则强制退出线程
                        logger.warn("Supervisor '" + name + "' join ChildTask '" + childName + "' timeout");
                    }
                }
                rest = true;
            }
        }
        rest = false;
        for (ChildInfo i : childList) {
            if (rest || i.name.equals(childName)) {
                Thread child = createChild(i);
                child.start();
                rest = true;
            }
        }
    }
}
