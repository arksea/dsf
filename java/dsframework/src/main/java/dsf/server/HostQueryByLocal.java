package dsf.server;

import java.net.InetAddress;
import java.net.UnknownHostException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * 获取本机IP
 * @author xiaohaixing_dian91
 */
public class HostQueryByLocal implements IHostQuery {
    private static final Logger logger = LogManager.getLogger(HostQueryByLocal.class);
    @Override
    public String query() {
        try {
            InetAddress addr = InetAddress.getLocalHost();
            final String host = addr.getHostAddress();
            logger.info("use local host address '{}'", host);
            return host;
        } catch (UnknownHostException ex) {
            throw new java.lang.IllegalStateException(ex);
        }
    }
}
