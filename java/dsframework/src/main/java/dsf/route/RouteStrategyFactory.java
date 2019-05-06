package dsf.route;

import dsf.register.Service;

/**
 * 路由策略工厂类，根据类型串选择路由策略，默认为带权重的轮询策略
 * @author arksea
 */
public class RouteStrategyFactory {

    public static IRouteStrategy create(String type, Service service) {
        if (type.equals("roundrobin")) {
            return new Roundrobin(service);
        } else if (type.equals("weighting-roundrobin")) {
            return new WeightingRoundrobin(service);
        } else if (type.equals("hot-standby")) {
            return new HotStandby(service);
        } else {
            return new WeightingRoundrobin(service);
        }
    }
}
