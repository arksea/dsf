package dsf.server;

import java.net.InetSocketAddress;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TThreadedSelectorServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;

/**
 *
 * @author arksea
 */
//子类需要声明ManagedResource注释：
//@ManagedResource(objectName = "com.baidu.softquery:name=ThriftServiceManager")
public class ThriftServiceManager extends AbstractServiceManager {
    
    @Override
    protected IThriftServerFactory createThriftServerFactory() {
        final String bindHost = this.getServiceBindHost();
        final int bindPort = this.getServiceBindPort();
        final InetSocketAddress addr = bindHost==null ? new InetSocketAddress(bindPort): new InetSocketAddress(bindHost, bindPort);
        IThriftServerFactory fac = new IThriftServerFactory() {
            @Override
            public TServer create() throws Throwable {
                TNonblockingServerTransport transport = new TNonblockingServerSocket(addr, 10000);
                return new TThreadedSelectorServer(new TThreadedSelectorServer.Args(transport)
                        .processor(processor)
                        .selectorThreads(selectorThreads)
                        .acceptQueueSizePerThread(acceptQueueSizePerThread)
                        .workerThreads(workerThreads));
            }
        };
        return fac;
    }
}
