package dsf.route;

import dsf.register.Service;

/**
 *
 * @author arksea
 */
public abstract class AbstractRouteStrategy implements IRouteStrategy {

    protected Service service;

    public AbstractRouteStrategy(Service svc) {
        this.service = svc;
    }
}
