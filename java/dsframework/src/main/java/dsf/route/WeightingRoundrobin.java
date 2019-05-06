package dsf.route;

import dsf.register.Service;
import dsf.register.ServiceInstance;
import java.util.List;

/**
 * 带权重的轮询路由策略，所有在线服务实例可实时更新自己的重复权重参数（重复调用次数）route-weight
 * 没有指定则默认为1
 * @author arksea
 */
public class WeightingRoundrobin extends AbstractRouteStrategy {

    private int index = 0; //当前引用的服务实例索引
    private int count = 0; //当前实例已调用的次数

    public WeightingRoundrobin(Service s) {
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
            String onlineStr = (String) s.getState("online");
            boolean online = (onlineStr != null && onlineStr.equals("true"));
            if (online) {
                ++onlineCount;
            }
            String weightStr = (String) s.getState("route-weight");
            int weight = 1;
            if (weightStr != null) {
                try {
                    weight = Integer.parseInt(weightStr);
                } catch (NumberFormatException numberFormatException) {
                }
            }
            if (online && ++count <= weight && s.tryAcquire()) {
                return s;
            } else {
                count = 0;
                ++index;
            }
        }
        if (onlineCount == 0) throw new NoUseableServiceException(service.define.name);
        throw new RateLimitedException(service.define.name);
    }
    
    public void next() {
        //换下一个实例
        count = 0;
        ++index;
    }
}
