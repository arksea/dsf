package dsf.route;

import dsf.register.Service;
import dsf.register.ServiceInstance;
import java.util.List;

/**
 * 轮询的路由策略，所有在线服务实例拥有相同的权重
 * @author arksea
 */
public class Roundrobin extends AbstractRouteStrategy {

    private int index = 0;

    public Roundrobin(Service s) {
        super(s);
    }

    @Override
    public ServiceInstance getInstance() {
        List<ServiceInstance> list = service.getInstanceList();
        int onlineCount = 0;
        for (int i = 0; i < list.size(); ++i) {
            if (index >= list.size()) {
                index = 0;
            }
            ServiceInstance s = list.get(index);
            ++index;
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
