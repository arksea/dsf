package dsf.server;

import akka.actor.ActorRef;
import akka.routing.SmallestMailboxPool;
import dsf.DSFramework;
import java.net.InetSocketAddress;
import org.apache.thrift.server.InvokeActor;
import org.apache.thrift.server.TSelectorActorServer;
import org.apache.thrift.server.TSelectorActorServer.Args.AcceptPolicy;
import org.apache.thrift.server.TServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;

/**
 *
 * @author arksea
 */
public class ActorServiceManager extends AbstractServiceManager {
    
    private boolean fastAcceptPolicy = false;
    
    protected ActorRef createExecutor() {
        if (workerThreads > 0) {
            return DSFramework.createActor(new SmallestMailboxPool(workerThreads).props(InvokeActor.props(processor)), "router-" + getServiceName());
        } else {
            return null;
        }
    }

    @Override
    protected IThriftServerFactory createThriftServerFactory() {
        final String bindHost = this.getServiceBindHost();
        final int bindPort = this.getServiceBindPort();
        final InetSocketAddress addr = bindHost == null ? new InetSocketAddress(bindPort) : new InetSocketAddress(bindHost, bindPort);
        executor = this.createExecutor();
        IThriftServerFactory fac = new IThriftServerFactory() {
            @Override
            public TServer create() throws Throwable {
                TNonblockingServerTransport transport = new TNonblockingServerSocket(addr, 10000);
                return new TSelectorActorServer(new TSelectorActorServer.Args(transport)
                        .processor(processor)
                        .executorService(executor)
                        .selectorThreads(selectorThreads)
                        .acceptQueueSizePerThread(acceptQueueSizePerThread)
                        .acceptPolicy(fastAcceptPolicy?AcceptPolicy.FAST_ACCEPT:AcceptPolicy.FAIR_ACCEPT)
                        .workerThreads(workerThreads));
            }
        };
        return fac;
    }

    @Override
    public void onAppStop() {
        super.onAppStop();
        if (executor != null) {
            DSFramework.stopActor(executor);
            executor = null;
        }
    }
    public void setFastAcceptPolicy(boolean is) {
        fastAcceptPolicy = is;
    }

    private volatile ActorRef executor;
}
