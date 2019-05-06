package dsf.pool;

import dsf.register.MsgServiceDefine;
import dsf.register.MsgSvcAddr;
import dsf.register.ServiceInstance;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 *
 * @author Administrator
 * @param <I>
 */
public class SpecifiedServerClientSource<I> extends ClientSource<I> {

    private final ServiceInstance instance;

    public SpecifiedServerClientSource(String regname, String host, int port, int timeout) {
        super(regname, timeout);
        instance = new ServiceInstance(new MsgSvcAddr(host, port));
        final MsgServiceDefine def = new MsgServiceDefine(regname, regname, "SpecifiedServerClientSource", "", new HashMap<String, String> ());
        instance.init(def);
    }

    @Override
    public RawClientInfo getRowClient() throws Exception {
        RawClientInfo raw = new RawClientInfo();
        raw.instance = this.instance;
        raw.client = creator.createClient(instance, timeout);
        return raw;
    }

    @Override
    public List<RawClientInfo> getRowClients() throws Exception {
        List<RawClientInfo> infos = new LinkedList<RawClientInfo>();
        RawClientInfo raw = new RawClientInfo();
        raw.instance = this.instance;
        raw.client = creator.createClient(raw.instance, timeout);
        infos.add(raw);
        return infos;
    }
}
