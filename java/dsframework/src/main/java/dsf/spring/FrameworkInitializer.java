package dsf.spring;

import dsf.DSFramework;
import dsf.adaptor.ServiceAdaptor;
import dsf.core.TaskCallException;
import java.net.InetSocketAddress;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 *
 * @author xiaohaixing_dian91
 */
public class FrameworkInitializer implements ApplicationListener<ContextRefreshedEvent> {

    @Override
    public void onApplicationEvent(ContextRefreshedEvent e) {
         if(e.getApplicationContext().getParent() == null){
            startApplication();
         }
    }

    private final static Logger logger = LogManager.getLogger(FrameworkInitializer.class.getName());

    private String registerServers;

    //登录名
    private String appName;
    private List<String> subscriptionList;

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
    
    public void startApplication() {
        startApplication(3000);
    }
    
    public void startApplication(long subscribeDelayMillis) {
        logger.info("启动DSFramework服务");
        InetSocketAddress[] addrs = parseSocketAddrArray(registerServers);
        DSFramework.start(addrs, appName, 30);
        if (subscriptionList != null) {
            for (String name: subscriptionList) {
                try {
                    ServiceAdaptor.subscribeService(name);
                } catch (TaskCallException ex) {
                    logger.debug("服务订阅失败：{}", name, ex);
                }
            }
            try {
            Thread.sleep(subscribeDelayMillis);
            } catch (InterruptedException ex) {
                logger.warn(ex);
            }
        }
    }

    public String getRegisterServers() {
        return registerServers;
    }

    public void setRegisterServers(String registerServers) {
        this.registerServers = registerServers;
    }


    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public void setSubscriptions(List<String> serviceList) {
        this.subscriptionList = serviceList;
    }
}
