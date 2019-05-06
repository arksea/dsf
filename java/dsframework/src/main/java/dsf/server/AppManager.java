package dsf.server;

import dsf.DSFramework;
import dsf.core.ChildInfo;
import dsf.core.RestartStrategies;
import dsf.core.TaskContext;
import dsf.resource.ResourceLeaksMonitor;
import java.net.InetSocketAddress;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;

/**
 *
 * @author arksea
 */
@ManagedResource(objectName = "dsf:name=dsf.server.AppManager")
public class AppManager implements ApplicationContextAware {

    private final static Logger logger = LogManager.getLogger(AppManager.class.getName());

    private String registerServers;

    //登录名
    private String appName;
    private long lastStopAppRequest = 0;
    private ApplicationContext applicationContext;
    private List<IServiceManager> serviceList;

    //从类似192.168.253.12:1033的配置串中提取host与port，并创建InetSocketAddress列表
    public static InetSocketAddress[] parseSocketAddrArray(String cfgStr) {
        String[] cfgList = cfgStr.split(";");
        int count = cfgList.length;
        InetSocketAddress[] addrs = new InetSocketAddress[count];
        for (int i = 0; i < count; ++i) {
            String cfg = cfgList[i];
            String[] strs = cfg.split(":");
            String host = strs[0];
            int port = Integer.parseInt(strs[1]);
            addrs[i] = new InetSocketAddress(host, port);
        }
        return addrs;
    }
    
    public void startApplication() throws InterruptedException {
        startApplication(3000,0);
    }
    
    public void startApplication(long registryDelayMillis,long serviceDelayMillis) throws InterruptedException {

        logger.info("启动应用进程: "+appName);
        InetSocketAddress[] addrs = parseSocketAddrArray(registerServers);
        DSFramework.start(addrs, appName, 30);
        ResourceLeaksMonitor.stopMonitor();        
        ChildInfo[] childInfos = new ChildInfo[1];
        Object args = DialedTask.createArgs();
        childInfos[0] = new ChildInfo(DialedTask.TASK_NAME, DialedTask.class, args);
        TaskContext.instance().start(DialedTask.TASK_NAME, RestartStrategies.ONE_FOR_ONE, childInfos);
        
        for (IServiceManager s: serviceList) {
            Thread.sleep(serviceDelayMillis);
            s.onAppStart();
        }
        Thread.sleep(registryDelayMillis);
        for (IServiceManager s: serviceList) {
            Thread.sleep(serviceDelayMillis);
            s.afterAppStart();
        }
        Thread stopThread = new Thread(appstoper,"app-shutdown-hook");
        Runtime.getRuntime().addShutdownHook(stopThread);
    }
    class AppStopper implements Runnable{
            @Override
            public void run() {
                try {
                    System.out.println("正在退出应用进程: "+appName);
                    //logger.warn("正在退出应用进程: "+appName);
                    //停止服务时用启动时的倒序
                    java.util.Collections.reverse(serviceList);
                    for (IServiceManager s: serviceList) {
                        s.onAppStop();
                    }
                    Thread.sleep(5000);
                    ResourceLeaksMonitor.stopMonitor();
                    DSFramework.stop();
                    if (applicationContext instanceof AbstractApplicationContext) {
                        ((AbstractApplicationContext) applicationContext).destroy();
                    }
                } catch (Throwable ex) {
                    logger.warn("退出应用进程时出错", ex);
                }
            }
    }
    private final AppStopper appstoper = new AppStopper();
    
    //退出服务进程
    @ManagedOperation
    public String stopApplication() {
        long now = System.currentTimeMillis();
        if (now - lastStopAppRequest > 5000) {
            lastStopAppRequest = now;
            logger.warn("收到退出应用进程的请求，5秒内再次收到此请求将执行退出进程操作");
            return "警告：在5秒内再次请求将退出应用进程！";
        }
        lastStopAppRequest = 0;
        Thread exitThread = new Thread("exit-thread") {
            @Override
            public void run() {
                System.exit(0);
            }
        };
        exitThread.setDaemon(true);
        exitThread.start();
        return "正在退出应用进程";
    }

    public void addServiceManager(IServiceManager s) {
        serviceList.add(s);
    }
    //启动资源泄露监控
    @ManagedOperation
    public void startResourceLeaksMonitor() {
        ResourceLeaksMonitor.startMonitor(30);
    }

    //停止资源泄露监控
    @ManagedOperation
    public void stopResourceLeaksMonitor() {
        ResourceLeaksMonitor.stopMonitor();
    }

//----------------------------------------------------------------------
// setter 与 getter
    @ManagedAttribute (description = "注册服务器地址列表")
    public String getRegisterServers() {
        return registerServers;
    }

    public void setRegisterServers(String registerServers) {
        this.registerServers = registerServers;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @ManagedAttribute
    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public void setServiceList(List<IServiceManager> serviceList) {
        this.serviceList = serviceList;
    }

}
