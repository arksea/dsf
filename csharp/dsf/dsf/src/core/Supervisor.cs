using System;

namespace com.baidu.dsf.core
{

    //using Logger = org.slf4j.Logger;
    //using LoggerFactory = org.slf4j.LoggerFactory;
    //using ImmutableList = com.google.common.collect.ImmutableList;

    /// <summary>
    /// 线程监控者，当线程ChildTask异常退出时负责将其重启
    /// $Author: xiaohaixing_298852 $
    /// </summary>
    public class Supervisor : java.lang.Thread.UncaughtExceptionHandler
    {
        public readonly string name;
        private readonly RestartStrategies restartStrategies;
        private readonly java.util.Map childMap = new java.util.HashMap();
        private readonly java.util.List childList; //保留子任务启动顺序
        private readonly long JOIN_CHILD_TIMEOUT = 5000;
        private long restartCount = 0;
        private long lastRestartTime = 0;

        protected internal Supervisor(string name, RestartStrategies restart, ChildInfo[] childs)
        {
            lock (this)
            {
                DSFLogHelper.DebugLog("Supervisor", "Supervisor '" + name + "' starting childs");
                this.name = name;
                this.restartStrategies = restart;
                childList = new java.util.ArrayList(childs.Length);
                for (int i = 0; i < childs.Length; ++i)
                {
                    childList.add(childs[i]);
                }
                startAll();
                DSFLogHelper.DebugLog("Supervisor", "Supervisor '" + name + "' started all childs");
            }
        }

        public ChildTask getChild(String childName)
        {
            return (ChildTask)childMap.get(childName);
        }

        protected internal virtual void stopAll()
        {
            lock (this)
            {
                DSFLogHelper.DebugLog("Supervisor", "Supervisor '" + name + "' stopping childs");
                java.util.Iterator it = childList.iterator();
                for (int i = childList.size() - 1; i >= 0; --i )
                {
                    ChildInfo info = (ChildInfo)childList.get(i);
                    ChildTask t = (ChildTask)childMap.get(info.name);
                    t.normalStopRequest();
                    TaskContext.instance().remove(info.name);
                    try
                    {
                        t.join(JOIN_CHILD_TIMEOUT);
                    }
                    catch (java.lang.InterruptedException)
                    {
                    }
                    if (t.isAlive())
                    {
                        t.stop(); //超时则强制退出线程
                    }
                }
                DSFLogHelper.DebugLog("Supervisor", "Supervisor '" + name + "' stopped all childs");
            }
        }

        private void startAll()
        {
            java.util.Iterator it = childList.iterator();
            while (it.hasNext())
            {
                ChildInfo c = (ChildInfo)it.next();
                createChild(c);
            }
            it = childList.iterator();
            while (it.hasNext())
            {
                ChildInfo c = (ChildInfo)it.next();
                ChildTask child = (ChildTask)childMap.get(c.name);
                child.start();
            }
        }

        private java.lang.Thread createChild(ChildInfo info)
        {
            try
            {
                ChildTask child;
                java.lang.Class mtClazz = (java.lang.Class)typeof(MessageChildTask);
                if (mtClazz.isAssignableFrom(info.clazz))
                {
                    java.lang.reflect.Constructor con = info.clazz.getConstructor(typeof(string), typeof(int), info.ArgsClass);
                    child = (ChildTask)con.newInstance(info.name, new java.lang.Integer(info.maxQueueLen), info.state);
                    child.setDaemon(info.Daemon);
                    TaskContext.instance().put(info.name, (MessageChildTask) child);
                } else {
                    java.lang.reflect.Constructor con = info.clazz.getConstructor((java.lang.Class)typeof(String), info.ArgsClass);
                    child = (ChildTask) con.newInstance(info.name, info.state);
                    child.setDaemon(info.Daemon);
                }
                child.setUncaughtExceptionHandler(this);
                childMap.put(info.name, child);
                return child;
            } catch (Exception ex) {
                throw new Exception("Supervisor '" + name + "' create ChildTask '" + info.name + "' failed", ex);
            }
        }


        public void uncaughtException(java.lang.Thread t, Exception ex)
        {
            string childName = t.getName();
            DSFLogHelper.ErrorLog("Supervisor", ex, "ChildTask '" + childName + "' exited because Exception");
            ++restartCount;
            long now = java.lang.System.currentTimeMillis();
            long time = now - lastRestartTime;
            lastRestartTime = now;
            if (time < 10000)
            {
                if (restartCount > 10)
                {
                    DSFLogHelper.ErrorLog("Supervisor", "ChileTask异常重启次数短时间内超过10次，将被停止：" + name);
                    TaskContext.instance().stop(name);
                }
            }
            else
            {
                restartCount = 1;
                switch (restartStrategies)
                {
                    case com.baidu.dsf.core.RestartStrategies.ONE_FOR_ONE:
                        stopOneForOne(childName);
                        break;
                    case com.baidu.dsf.core.RestartStrategies.ONE_FOR_ALL:
                        stopOneForAll(childName);
                        break;
                    case com.baidu.dsf.core.RestartStrategies.REST_FOR_ONE:
                        stopRestForOne(childName);
                        break;
                }
            }
        }

        private void stopOneForOne(string childName)
        {
            java.util.Iterator it = childList.iterator();
            while(it.hasNext())
            {
                ChildInfo i = (ChildInfo)it.next();
                if (i.name.Equals(childName))
                {
                    TaskContext.instance().remove(childName);
                    java.lang.Thread child = createChild(i);
                    child.start();
                    break;
                }
            }
        }

        private void stopOneForAll(string childName)
        {
            java.util.Iterator it = childList.iterator();
            while (it.hasNext())
            {
                ChildInfo i = (ChildInfo)it.next();
                TaskContext.instance().remove(childName);
                ChildTask child = (ChildTask)childMap.get(i.name);
                child.normalStopRequest();
                try
                {
                    child.join(JOIN_CHILD_TIMEOUT);
                }
                catch (java.lang.InterruptedException ex)
                {
                    DSFLogHelper.WarnLog("Supervisor", ex, "Supervisor '" + name + "' join ChildTask '" + childName + "' interruped");
                }
                if (child.isAlive())
                {
                    child.stop(); //超时则强制退出线程
                    DSFLogHelper.WarnLog("Supervisor", "Supervisor '" + name + "' join ChildTask '" + childName + "' timeout");
                }
            }
            it = childList.iterator();
            while (it.hasNext())
            {
                ChildInfo i = (ChildInfo)it.next();
                java.lang.Thread child = createChild(i);
                child.start();
            }
        }

        private void stopRestForOne(string childName)
        {
            bool rest = false;
            java.util.Iterator it = childList.iterator();
            while (it.hasNext())
            {
                ChildInfo i = (ChildInfo)it.next();
                if (rest || i.name.Equals(childName))
                {
                    if (rest)
                    {
                        TaskContext.instance().remove(childName);
                        ChildTask child = (ChildTask)childMap.get(i.name);
                        child.normalStopRequest();
                        try
                        {
                            child.join(JOIN_CHILD_TIMEOUT);
                        }
                        catch (java.lang.InterruptedException ex)
                        {
                            DSFLogHelper.WarnLog("Supervisor", ex, "Supervisor '" + name + "' join ChildTask '" + childName + "' interruped");
                        }
                        if (child.isAlive())
                        {
                            child.stop(); //超时则强制退出线程
                            DSFLogHelper.WarnLog("Supervisor", "Supervisor '" + name + "' join ChildTask '" + childName + "' timeout");
                        }
                    }
                    rest = true;
                }
            }
            rest = false;
            it = childList.iterator();
            while (it.hasNext())
            {
                ChildInfo i = (ChildInfo)it.next();
                if (rest || i.name.Equals(childName))
                {
                    java.lang.Thread child = createChild(i);
                    child.start();
                    rest = true;
                }
            }
        }
    }

}