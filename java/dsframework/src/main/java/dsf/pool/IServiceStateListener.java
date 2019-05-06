package dsf.pool;

import dsf.register.MsgSvcAddr;
import dsf.register.MsgSvcState;
import java.util.Collection;

/**
 *
 * @author xiaohaixing_dian91
 */
public interface IServiceStateListener {
    //根据回报的服务器状态，删除已经offline的Pool
    void notifySvcStates(MsgSvcAddr addr, Collection<MsgSvcState> states);
    String getServiceRegname();
}
