namespace com.baidu.dsf.register
{

    using FailStrategy = com.baidu.dsf.pool.FailStrategy;
    using IRouteStrategy = com.baidu.dsf.route.IRouteStrategy;
    using RouteStrategyFactory = com.baidu.dsf.route.RouteStrategyFactory;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class Service
    {

        public readonly MsgServiceDefine define;
        public readonly IRouteStrategy routeStrategy;
        public readonly FailStrategy failStrategy;
        protected internal readonly java.util.List instanceList = new java.util.LinkedList();

        public Service(MsgSubscribeResult ret)
        {
            this.define = ret.svcdef;
            string fs = this.define.getProperty("fail_strategy");
            if (fs != null)
            {
                switch (fs)
                {
                    case "failfast":
                        this.failStrategy = FailStrategy.FAILFAST;
                        break;
                    case "failover":
                        this.failStrategy = FailStrategy.FAILOVER;
                        break;
                    case "failsafe":
                        this.failStrategy = FailStrategy.FAILSAFE;
                        break;
                    default:
                        this.failStrategy = FailStrategy.FAILFAST;
                        break;
                }
            }
            else
            {
                this.failStrategy = FailStrategy.FAILFAST;
            }
            foreach (ServiceInstance inst in ret.svclist)
            {
                inst.init(define);
                instanceList.add(inst);
            }
            routeStrategy = RouteStrategyFactory.create(this.define.getProperty("route_strategy"), this);
        }

        public virtual java.util.List InstanceList
        {
            get
            {
                return java.util.Collections.unmodifiableList(instanceList);
            }
        }

        public virtual void updateDefine(MsgServiceDefine o)
        {
            define.update(o);
        }

        public virtual ServiceInstance Instance
        {
            get
            {
                return routeStrategy.Instance;
            }
        }

        public virtual void updateStates(MsgSvcAddr addr, System.Collections.Generic.ICollection<MsgSvcState> states)
        {
            java.util.Iterator it = instanceList.iterator();
            while(it.hasNext())
            {
                ServiceInstance i = (ServiceInstance)it.next();
                if (i.Addr.Equals(addr))
                {
                    foreach (MsgSvcState s in states)
                    {
                        i.setState(s.name, s.value);
                    }
                    return;
                }
            }

            ServiceInstance ni = new ServiceInstance(addr);
            foreach (MsgSvcState s in states)
            {
                ni.setState(s.name, s.value);
            }
            ni.init(define);
            instanceList.add(ni);
        }
    }

}