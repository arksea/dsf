package dsf.register;

import dsf.pool.FailStrategy;
import dsf.pool.IServiceStateListener;
import dsf.route.IRouteStrategy;
import dsf.route.RouteStrategyFactory;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author arksea
 */
public class Service {

    public final MsgServiceDefine define;
    public final IRouteStrategy routeStrategy;
    public final FailStrategy failStrategy;
    private final List<ServiceInstance> instanceList = new LinkedList<>();
    private final List<IServiceStateListener> serviceStateListenerList = new LinkedList<>();
    
    public Service(MsgSubscribeResult ret) {
        this.define = ret.svcdef;
        String fs = this.define.getProperty("fail_strategy");
        if (fs != null) {
            if (fs.equals("failfast")) {
                    this.failStrategy = FailStrategy.FAILFAST;
            } else if (fs.equals("failover")) {
                    this.failStrategy = FailStrategy.FAILOVER;
            } else if (fs.equals("failsafe")) {
                    this.failStrategy = FailStrategy.FAILSAFE;
            } else {
              this.failStrategy = FailStrategy.FAILFAST;
            }
        } else {
            this.failStrategy = FailStrategy.FAILFAST;
        }
        for (ServiceInstance inst : ret.svclist) {
            inst.init(define);
            instanceList.add(inst);
        }
        routeStrategy = RouteStrategyFactory.create(this.define.getProperty("route_strategy"), this);
    }
    
    public List getInstanceList() {
        return java.util.Collections.unmodifiableList(instanceList);
    }
    
    public void updateDefine(MsgServiceDefine o) {
        define.update(o);
    }

    public ServiceInstance getInstance() {
        return routeStrategy.getInstance();
    }

    public void updateStates(MsgSvcAddr addr, Collection<MsgSvcState> states) {
        for (IServiceStateListener l : serviceStateListenerList) {
            l.notifySvcStates(addr, states);
        }
        for (ServiceInstance i : instanceList) {
            if (i.getAddr().equals(addr)) {
                for (MsgSvcState s : states) {
                    i.setState(s.name, s.value);
                }
                return;
            }
        }

        ServiceInstance ni = new ServiceInstance(addr);
        for (MsgSvcState s : states) {
            ni.setState(s.name, s.value);
        }
        ni.init(define);
        instanceList.add(ni);
    }
    
    public void registerStateListener(final IServiceStateListener l) {
        serviceStateListenerList.add(l);
    }
    
    public void unregisterStateListener(final IServiceStateListener l) {
        serviceStateListenerList.remove(l);
    }
}
