package dsf;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import java.net.InetSocketAddress;

import dsf.core.ChildInfo;
import dsf.core.RestartStrategies;
import dsf.core.TaskContext;
import dsf.register.RegisterClient;
import dsf.register.ServiceManager;
import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * 服务框架入口
 * @author arksea
 */
public class DSFramework {

    private static final DSFramework app = new DSFramework();
    private static boolean started = false;
    private static ActorSystem system;
    private static Config config;

    private DSFramework() {
    }

    /**
     * 启动框架
     * @param addrs                     注册服务器地址列表
     * @param loginName                 注册服务器登录名，作客户端标示
     */
    public static synchronized void start(InetSocketAddress[] addrs, String loginName) {
        start(addrs, loginName, 30);
    }

    /**
     * 启动框架
     * @param addrs                     注册服务器地址列表
     * @param loginName                 注册服务器登录名，作客户端标示
     * @param maxQueueSize              最大消息队列长度
     */
    public static synchronized void start(InetSocketAddress[] addrs, String loginName, int maxQueueSize) {
        if (!DSFramework.started) {
            app.onStart(addrs, loginName, maxQueueSize);
            DSFramework.started = true;
        }
    }

    private void onStart(InetSocketAddress[] addrs, String loginName, int maxQueueSize) {
        config = ConfigFactory.load();
        system = ActorSystem.create("DSFramework",config);
        if (addrs == null || addrs.length == 0) {
            throw new RuntimeException("注册服务器列表不能为空");
        }
        if (loginName == null || loginName.length() == 0) {
            throw new RuntimeException("注册服务器登录名不能为空");
        }

        TaskContext.instance().stopAll();
        ChildInfo[] childs = new ChildInfo[2];

        // 注册服务器客户端线程
        childs[0] = RegisterClient.createChildInfo(loginName, addrs, maxQueueSize);
        // 服务管理线程
        childs[1] = ServiceManager.createChildInfo(maxQueueSize);
        TaskContext.instance().start("register_sup", RestartStrategies.ONE_FOR_ONE, childs);
    }

    public static synchronized void stop() {
        TaskContext.instance().stopAll();
        system.shutdown();
        DSFramework.started = false;
    }

    public static synchronized boolean isStarted() {
        return DSFramework.started;
    }
    
    public static ActorRef createActor(Props props, String name) {
        return system.actorOf(props, name);
    }
    
    public static void stopActor(ActorRef actor) {
        system.stop(actor);
    }
    
    public static void stopActorSystem() {
        system.shutdown();
    }
}
