package dsf.server;

import java.net.InetAddress;
import java.net.UnknownHostException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.client.HttpComponentsClientHttpRequestFactory;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

/**
 * 通过指定URL获取Hostname
 * @author xiaohaixing_dian91
 */
public class HostQueryByUrl implements IHostQuery {
    private final String url;
    private static final Logger logger = LogManager.getLogger(HostQueryByUrl.class);
    HostQueryByUrl(String url) {
        this.url = url;
    }
    @Override
    public String query() {
        try {
            RestTemplate template = new RestTemplate(new HttpComponentsClientHttpRequestFactory());
            String host = template.getForObject(url, String.class);
            logger.info("responded host '{}' from url {}", host, url);
            return host;
        } catch (RestClientException ex) {
            try {
                logger.error("query hostname from url failed,use the local host address, url={}",ex,url);
                return InetAddress.getLocalHost().getHostAddress();
            } catch (UnknownHostException ex1) {
                throw new java.lang.IllegalStateException("query hostname from url failed, get local address failed too!", ex1);
            }
        }
    }
}
