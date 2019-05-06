package dsf.pool;

import dsf.register.MsgSvcAddr;

/**
 *
 * @author arksea
 */
public class ClientSourceException extends RuntimeException {

    private String regname;
    private MsgSvcAddr svcAddr;

    public ClientSourceException(String msg, String regname, MsgSvcAddr addr) {
        super(msg);
        this.regname = regname;
        this.svcAddr = addr;
    }

    public ClientSourceException(String msg, String regname, MsgSvcAddr addr, Throwable ex) {
        super(msg, ex);
        this.regname = regname;
        this.svcAddr = addr;
    }

    public String getRegname() {
        return regname;
    }

    public MsgSvcAddr getSvcAddr() {
        return svcAddr;
    }
}
