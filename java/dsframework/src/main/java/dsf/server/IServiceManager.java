package dsf.server;

/**
 *
 * @author xhx
 */
public interface IServiceManager {
    void onAppStart();
    void afterAppStart();
    void onAppStop();
}
