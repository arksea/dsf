package dsf.thrift;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TTransport;

/**
 *
 * @author arksea
 */
public class ProtocolFactoryUtil {

    public static TProtocolFactory getFactory(String type) {
        switch (type) {
            case "binary":
                return new TProtocolFactory() {
                    @Override
                    public TProtocol getProtocol(TTransport transport) {
                        TTagFailedTransport tt = new TTagFailedTransport(transport);
                        return new TBinaryProtocol(tt);
                    }
                };
            case "framed-binary":
                return new TProtocolFactory() {
                    @Override
                    public TProtocol getProtocol(TTransport transport) {
                        TTagFailedTransport tt = new TTagFailedTransport(new TFramedTransport(transport));
                        return new TBinaryProtocol(tt);
                    }
            };
            case "zip-binary":
                return new TProtocolFactory() {
                    @Override
                    public TProtocol getProtocol(TTransport transport) {
                        TTagFailedTransport tt = new TTagFailedTransport(new TZlibTransport(transport));
                        return new TBinaryProtocol(tt);
                    }
                };
            case "compact":
                return new TProtocolFactory() {
                    @Override
                    public TProtocol getProtocol(TTransport transport) {
                        TTagFailedTransport tt = new TTagFailedTransport(transport);
                        return new TCompactProtocol(tt);
                    }
                };
            case "framed-compact":
                return new TProtocolFactory() {
                    @Override
                    public TProtocol getProtocol(TTransport transport) {
                        TTagFailedTransport tt = new TTagFailedTransport(new TFramedTransport(transport));
                        return new TCompactProtocol(tt);
                    }
                };
            default:
                return new TProtocolFactory() {
                    @Override
                    public TProtocol getProtocol(TTransport transport) {
                        TTagFailedTransport tt = new TTagFailedTransport(new TFramedTransport(transport));
                    return new TBinaryProtocol(tt);
                }
            };
        }
    }
}
