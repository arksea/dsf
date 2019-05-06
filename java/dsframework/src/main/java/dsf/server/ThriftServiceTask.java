package dsf.server;

import dsf.core.ChildTask;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.server.ServerContext;
import org.apache.thrift.transport.TTransport;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TServerEventHandler;

/**
 * 应用服务端运行线程，用于将应用服务纳入监控树中，
 * 当应用服务异常退出时将被Supervisor重启
 * @author arksea
 * @param <T>
 */
public class ThriftServiceTask<T extends IThriftServerFactory> extends ChildTask<T> implements TServerEventHandler {

    private final static Logger logger = LogManager.getLogger(ThriftServiceTask.class.getName());
    private volatile boolean normalStop = false;
    private TServer server;
    private final AtomicInteger connectionCount = new AtomicInteger(0);
    private final AtomicLong requestCount = new AtomicLong(0L);

    @Override
    public void preServe() {
    }

    @Override
    public ServerContext createContext(TProtocol input, TProtocol output) {
        connectionCount.incrementAndGet();
        return new ServerContext() {
        };
    }

    @Override
    public void deleteContext(ServerContext serverContext, TProtocol input, TProtocol output) {
        connectionCount.decrementAndGet();
    }

    @Override
    public void processContext(ServerContext serverContext, TTransport inputTransport, TTransport outputTransport) {
        requestCount.incrementAndGet();
    }

    public ThriftServiceTask(String name, T args) {
        super(name, args);
    }

    @Override
    public void run() {
        try {
            synchronized (this) {
                server = state.create();
                server.setServerEventHandler(this);
            }
            logger.trace("service task " + getName() + " is started");
            if (!normalStop) {
                server.serve();
            }
            if (normalStop) {
                logger.trace("service task " + getName() + " is stopped");
            } else {
                throw new RuntimeException("service task " + getName() + " failed");
            }
        } catch (Throwable ex) {
            throw new RuntimeException("service task " + getName() + " failed", ex);
        }
    }

    @Override
    public void normalStopRequest() {
        synchronized (this) {
            normalStop = true;
            if (server != null) {
                server.stop();
            }
        }
    }

    public int getConnectionCount() {
        return connectionCount.get();
    }
    
    public long getRequestCount() {
        return requestCount.get();
    }
}
