package dsf.pool;

import java.util.List;

/**
 *
 * @author arksea
 */
public abstract class AbstractProxyClientSource<I> implements IClientSource<I> {
    protected String regname;
    abstract RawClientInfo getRowClient() throws Exception;
    abstract List<RawClientInfo> getRowClients() throws Exception;
    abstract void returnRowClient(RawClientInfo p); //归还一个客户端
}
