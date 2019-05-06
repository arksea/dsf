package dsf.server;

import dsf.adaptor.ServiceAdaptor;
import dsf.core.ChildInfo;
import dsf.core.Message;
import dsf.core.RestartStrategies;
import dsf.core.Supervisor;
import dsf.core.TaskContext;
import dsf.register.MsgServiceReg;
import dsf.register.MsgSvcState;
import dsf.register.RegisterClient;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;
import org.apache.thrift.TProcessor;
import org.apache.thrift.TServiceClient;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.thrift.TBaseAsyncProcessor;
import org.apache.thrift.async.AsyncMethodCallback;
import org.apache.thrift.async.TAsyncClient;
import org.apache.thrift.async.TAsyncClientManager;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TNonblockingSocket;
import org.apache.thrift.transport.TNonblockingTransport;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;

/**
 *
 * @author arksea
 */
//子类需要声明ManagedResource注释：
//@ManagedResource(objectName = "com.baidu.softquery:name=ThriftServiceManager")
public abstract class AbstractServiceManager implements IServiceManager {

    protected final static Logger logger = LogManager.getLogger(AbstractServiceManager.class.getName());
    protected String serviceName = "not setted";
    protected String version = "v1.0";

    protected volatile boolean isStop = false;

    //服务绑定地址
    private int serviceBindPort;
    private String serviceBindHost;
    private IHostQuery bindHostQuery;
    private String serviceRegisterHost;
    private IHostQuery registerHostQuery;
    private int managePort;

    //注册服务重试延时
    private long retryDelay = 1000;
    private long minRetryDelay = 1000;
    private long maxRetryDelay = 30000;

    //线程池配置
    protected volatile int selectorThreads = 20;
    protected volatile int acceptQueueSizePerThread = 10;
    protected volatile int workerThreads = 100;

    private volatile boolean isRegistered = false;
    private volatile long lastStopServiceRequest = 0;
    
    private String dialedMethod = null;
    private boolean dialedFailed = false;
    private boolean dialedSucceed = false;
    private boolean autoRegisterAfterAppStart = true;
    private boolean asyncService = false;
    private TAsyncClientManager asyncClientManager;
    protected volatile Supervisor supervisor;

    protected TProcessor processor;
    protected Object serviceHandler;

    protected abstract IThriftServerFactory createThriftServerFactory();
    
    public void setServiceHandler(Object handler) {
        if (serviceHandler == null) {
            try {
            this.serviceHandler = handler;
            asyncService = false;
            Class<?> clientClass = Class.forName(getServiceName() + "$Processor");
            Constructor con = clientClass.getConstructor(Class.forName(getServiceName()+"$Iface"));
            processor = (TProcessor) con.newInstance(handler);
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        } else {
            throw new RuntimeException("Service Handler Setted");
        }
    }

    public void setAsyncServiceHandler(Object handler) {
        if (serviceHandler == null) {
            try {
            this.serviceHandler = handler;
            asyncService = true;
            Class<?> processorClass = Class.forName(getServiceName() + "$AsyncProcessor");
            Constructor con = processorClass.getConstructor(Class.forName(getServiceName()+"$AsyncIface"));
            processor = (TBaseAsyncProcessor) con.newInstance(handler);
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        } else {
            throw new RuntimeException("Service Handler Setted");
        }
    }

    //DialedTask定时回调此方法，用于服务连通性测试，默认间隔30秒
    public void onDialed() {
        if (dialedMethod == null || !isIsRegistered()) return;
        final String bindHost = this.getServiceBindHost();
        String regHost = this.getServiceRegisterHost();
        String testHost = bindHost==null ? "127.0.0.1" : bindHost;
        int port = getServiceBindPort();
        MsgServiceReg svcreg = new MsgServiceReg(getRegname(), regHost, port);
        try {
            if (asyncService) {
                asyncDialed(testHost, port, svcreg);
            } else {
                syncDialed(testHost, port);
                if (!dialedSucceed) {
                    logger.info("服务在线状态拨测成功："+getRegname());
                    dialedSucceed = true;
                    dialedFailed = false;
                }
                try {
                    ServiceAdaptor.reportServiceState(svcreg, "online", "true");
                } catch (Exception ex) {
                    logger.warn("报告服务状态失败", ex);
                }
            }
        } catch(Throwable ex) {
            if (!dialedFailed) {
                logger.warn("服务在线状态拨测失败："+getRegname(), ex);
                dialedFailed = true;
                dialedSucceed = false;
            }
            try {
            ServiceAdaptor.reportServiceState(svcreg, "online", "false");
            } catch (Exception ex1) {
                logger.warn("报告服务状态失败", ex);
            }
        }
    }
    private void syncDialed(String testHost, int port) throws Exception {
        TTransport transport = null;
        try {
            TSocket sock = new TSocket(testHost, port, 10000);
            transport = new TFramedTransport(sock);
            transport.open();
            TProtocol protocol = new TBinaryProtocol(transport);
            String className = getServiceName() + "$Client";
            Class<?> clientClass = Class.forName(className);
            Constructor con = clientClass.getConstructor(TProtocol.class);
            TServiceClient client = (TServiceClient) con.newInstance(protocol);
            Method method = clientClass.getMethod(dialedMethod);
            method.invoke(client);
        } finally {
          if (transport != null) transport.close();
        }
    }
    private void asyncDialed(String testHost, int port, final MsgServiceReg svcreg) throws Exception {
        TNonblockingSocket transport = null;
        try {
            transport = new TNonblockingSocket(testHost, port, 10000);
            TProtocolFactory protocolFactory = new TBinaryProtocol.Factory();
            String className = getServiceName() + "$AsyncClient";
            Class<?> clientClass = Class.forName(className);
            Constructor con = clientClass.getConstructor(TProtocolFactory.class, TAsyncClientManager.class, TNonblockingTransport.class);
            TAsyncClient client = (TAsyncClient) con.newInstance(protocolFactory, asyncClientManager, transport);
            Method method = clientClass.getMethod(dialedMethod, AsyncMethodCallback.class);
            final TNonblockingSocket socket = transport;
            method.invoke(client, new AsyncMethodCallback() {
                @Override
                public void onComplete(Object t) {
                    close();
                    if (!dialedSucceed) {
                        logger.info("服务在线状态拨测成功："+getRegname());
                        dialedSucceed = true;
                        dialedFailed = false;
                    }
                    try {
                        ServiceAdaptor.reportServiceState(svcreg, "online", "true");
                    } catch (Exception ex) {
                        logger.warn("报告服务状态失败", ex);
                    }
                }

                @Override
                public void onError(Exception ex) {
                    close();
                    if (!dialedFailed) {
                        logger.warn("服务在线状态拨测失败："+getRegname(), ex);
                        dialedFailed = true;
                        dialedSucceed = false;
                    }
                    try {
                        ServiceAdaptor.reportServiceState(svcreg, "online", "false");
                    } catch (Exception ex1) {
                        logger.warn("报告服务状态失败", ex1);
                    }
                }

                private void close() {
                    if (socket != null) socket.close();
                }
            });
        } catch(Exception ex) {
            if (transport != null) transport.close();
            throw ex;
        }
    }
    private long lastRequestCount = 0L;
    private long lastStatTime = System.currentTimeMillis();
    private final AtomicLong qps = new AtomicLong(0L);
    //DialedTask定时回调此方法，用于性能统计，默认间隔5秒
    public void onStat() {
        long count = 0L;
        if (supervisor!=null) {
            ThriftServiceTask t = (ThriftServiceTask)supervisor.getChild(getRegname());
            if (t!=null) {
                count = t.getRequestCount();
            }
        }
        
        long now = System.currentTimeMillis();
        long q = (lastRequestCount - count)*1000/(lastStatTime - now);
        lastRequestCount = count;
        lastStatTime = now;
        qps.set(q);
    }
    
    private void delayForRetry() {
        try {
            Thread.sleep(retryDelay);
            retryDelay *= 2;
            if (retryDelay > maxRetryDelay) {
                retryDelay = maxRetryDelay;
            }
        } catch (InterruptedException ex) {
            throw new RuntimeException("等待重试时被中断：" + getRegname(), ex);
        }
    }
    @Override
    public void onAppStart() {
        startService();
    }
   
    @ManagedOperation
    public void startService() {
        try {
            asyncClientManager = new TAsyncClientManager();
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
        IThriftServerFactory fac = createThriftServerFactory();
        ChildInfo[] childInfos = new ChildInfo[1];
        childInfos[0] = new ChildInfo(getRegname(), ThriftServiceTask.class, fac);
        childInfos[0].setArgsClass(IThriftServerFactory.class);
        supervisor = TaskContext.instance().start(getRegname(), RestartStrategies.ONE_FOR_ONE, childInfos);
        logger.info("应用服务已启动：" + getRegname());
        isStop = false;
    }
    
    @Override
    public void onAppStop() {
        stopService(false);
        //asyncClientManager.stop();
    }
    
    @Override
    public void afterAppStart() {
        if (autoRegisterAfterAppStart) {
            registerService(true);
        }
    }
    
    @ManagedOperation
    public void registerService() {
        registerService(false);
    }

    public void registerService(boolean retry) {
        MsgServiceReg svcreg = new MsgServiceReg(getRegname(), this.getServiceRegisterHost(), serviceBindPort);
        do {
            try {
                ServiceAdaptor.registerService(svcreg);
                List<MsgSvcState> states = new LinkedList<MsgSvcState>();
                states.add(new MsgSvcState("manage_port",""+managePort));
                states.add(new MsgSvcState("manage_protocol","jmx"));
                RegisterClient.setServiceState(svcreg, states);
                TaskContext.instance().send(DialedTask.TASK_NAME, new Message("register", this));
                logger.info("应用服务已注册：" + getRegname());
                retryDelay = minRetryDelay;
                isRegistered = true;
                return;
            } catch (Throwable ex) {
                logger.warn("注册应用服务失败：" + getRegname(), ex);
            }
            if (retry) {
                delayForRetry();
            }
        } while (retry && !isStop);
    }

    @ManagedOperation
    public void unregisterService() {
        unregisterService(false);
    }

    public void unregisterService(boolean retry) {
        MsgServiceReg svcreg = new MsgServiceReg(getRegname(), this.getServiceRegisterHost(), serviceBindPort);
        do {
            try {
                ServiceAdaptor.unregisterService(svcreg);
                TaskContext.instance().send(DialedTask.TASK_NAME, new Message("unregister", this));
                System.err.println("应用服务已注销：" + getRegname());
                //logger.warn("应用服务已注销：" + getRegname());
                retryDelay = minRetryDelay;
                isRegistered = false;
                return;
            } catch (Throwable ex) {
                System.err.println("注销应用服务失败：" + getRegname());
                //logger.warn("注销应用服务失败：" + getRegname(), ex);
            }
            if (retry) {
                delayForRetry();
            }
        } while (retry && !isStop);
    }

    @ManagedOperation
    public String stopService() {
        return stopService(true);
    }

    public String stopService(boolean confirm) {
        long now = System.currentTimeMillis();

        if (confirm && (now - lastStopServiceRequest > 5000)) {
            lastStopServiceRequest = now;
            logger.warn("收到停止应用服务的请求，5秒内再次收到此请求将执行停止服务操作：" + getRegname());
            return "警告：在5秒内再次请求将停止应用服务！";
        }
        isStop = true;
        lastStopServiceRequest = 0;
        unregisterService(false);
        System.err.println("正在停止应用服务：" + getRegname());
        //logger.warn("正在停止应用服务：" + getRegname());
        TaskContext.instance().stop(getRegname());
        supervisor = null;
        return "已停止应用服务";
    }

//----------------------------------------------------------------------
// setter 与 getter

    @ManagedAttribute (description = "服务注册最大重试延时时间")
    public long getMaxRetryDelay() {
        return maxRetryDelay;
    }

    public void setMaxRetryDelay(long maxRetryDelay) {
        this.maxRetryDelay = maxRetryDelay;
    }

    @ManagedAttribute (description = "服务注册最小重试延时时间")
    public long getMinRetryDelay() {
        return minRetryDelay;
    }

    @ManagedOperation
    public void setMinRetryDelay(long minRetryDelay) {
        this.minRetryDelay = minRetryDelay;
    }

    @ManagedAttribute
    public String getServiceBindHost() {
        return serviceBindHost;
    }

    public void setServiceBindHost(String serviceBindHost) {
        this.serviceBindHost = serviceBindHost;
    }
    
    @ManagedAttribute
    public String getServiceRegisterHost() {
        if (serviceRegisterHost == null) {
            try {
                InetAddress addr = InetAddress.getLocalHost();
                serviceRegisterHost = addr.getHostAddress();
                logger.info("use local host address to register service'{}'", serviceRegisterHost);

            } catch (UnknownHostException ex) {
                throw new java.lang.IllegalStateException(ex);
            }
        }
        return serviceRegisterHost;
    }

    public void setServiceRegisterHost(String serviceRegisterHost) {
        this.serviceRegisterHost = serviceRegisterHost;
    }

    @ManagedAttribute
    public int getServiceBindPort() {
        return serviceBindPort;
    }

    public void setServiceBindPort(int serviceBindPort) {
        this.serviceBindPort = serviceBindPort;
    }
    
    @ManagedAttribute
    public int getManagePort() {
        return managePort;
    }

    public void setManagePort(int managePort) {
        this.managePort = managePort;
    }

    @ManagedAttribute
    public boolean isIsRegistered() {
        return isRegistered;
    }

    @ManagedAttribute
    public int getAcceptQueueSizePerThread() {
        return acceptQueueSizePerThread;
    }

    @ManagedOperation
    public void setAcceptQueueSizePerThread(int acceptQueueSizePerThread) {
        this.acceptQueueSizePerThread = acceptQueueSizePerThread;
    }

    @ManagedAttribute
    public int getSelectorThreads() {
        return selectorThreads;
    }

    @ManagedOperation
    public void setSelectorThreads(int selectorThreads) {
        this.selectorThreads = selectorThreads;
    }

    @ManagedAttribute
    public int getWorkerThreads() {
        return workerThreads;
    }

    @ManagedOperation
    public void setWorkerThreads(int workerThreads) {
        this.workerThreads = workerThreads;
    }

    @ManagedAttribute
    public String getServiceName() {
        return serviceName;
    }

    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }
    
    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    @ManagedAttribute (description = "服务注册名")
    public String getRegname() {
        return serviceName+" "+version;
    }
    
    @ManagedAttribute (description = "客户端连接数")
    public int getConnectionCount() {
        if (supervisor!=null) {
            ThriftServiceTask t = (ThriftServiceTask)supervisor.getChild(getRegname());
            if (t!=null) {
                return t.getConnectionCount();
            }
        }
        return 0;
    }
    
    @ManagedAttribute (description = "总请求数")
    public long getRequestCount() {
        if (supervisor!=null) {
            ThriftServiceTask t = (ThriftServiceTask)supervisor.getChild(getRegname());
            if (t!=null) {
                return t.getRequestCount();
            }
        }
        return 0L;
    }
    
    @ManagedAttribute (description = "每秒请求数(QPS)")
    public long getQPS() {
        return qps.get();
    }

    @ManagedAttribute (description = "服务在线状态拨测方法")
    public String getDialedMethod() {
        return dialedMethod;
    }

    public void setDialedMethod(String dialedMethod) {
        this.dialedMethod = dialedMethod;
    }
    
    @ManagedAttribute (description = "是否异步服务")
    public boolean isAsyncService() {
        return asyncService;
    }
    
    @ManagedAttribute (description = "是否在应用启动后立即注册服务")
    public boolean isAutoRegisterAfterAppStart() {
        return autoRegisterAfterAppStart;
    }

    public void setAutoRegisterAfterAppStart(boolean autoRegisterAfterAppStart) {
        this.autoRegisterAfterAppStart = autoRegisterAfterAppStart;
    }

    public IHostQuery getBindHostQuery() {
        return bindHostQuery;
    }

    public void setBindHostQuery(IHostQuery bindHostQuery) {
        this.bindHostQuery = bindHostQuery;
        this.serviceBindHost = bindHostQuery.query();
    }

    public IHostQuery getRegisterHostQuery() {
        return registerHostQuery;
    }

    public void setRegisterHostQuery(IHostQuery regHostQuery) {
        this.registerHostQuery = regHostQuery;
        this.serviceRegisterHost = regHostQuery.query();
    }

}
