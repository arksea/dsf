package dsf.server;

/**
 * 通过指定URL获取Hostname
 * @author xiaohaixing_dian91
 */
public class HostQueryByString implements IHostQuery {
    private final String hostname;
    HostQueryByString(String hostname) {
        this.hostname = hostname;
    }
    @Override
    public String query() {
        return hostname;
    }
}
