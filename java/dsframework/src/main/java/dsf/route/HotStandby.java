package dsf.route;

import dsf.register.Service;
import dsf.register.ServiceInstance;
import java.util.List;
import java.util.TreeSet;

/**
 * 热备的路由策略，按服务器地址排序，选择第一个在线的服务
 * @author arksea
 */
public class HotStandby extends AbstractRouteStrategy {
    
    public HotStandby(Service s) {
        super(s);
    }

    @Override
    public ServiceInstance getInstance() {
        List<ServiceInstance> list = service.getInstanceList();
        TreeSet<ServiceInstance> sorted = new TreeSet();
        sorted.addAll(list);
        int onlineCount = 0;
        for (ServiceInstance s : sorted) {
            String onlineStr = (String) s.getState("online");
            boolean online = (onlineStr != null && onlineStr.equals("true"));
            if (online) {
                ++onlineCount;
                if (s.tryAcquire()) {
                    return s;
                }
            }
        }
        if (onlineCount == 0) throw new NoUseableServiceException(service.define.name);
        throw new RateLimitedException(service.define.name);
    }
}
