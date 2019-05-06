package dsf.register;

import com.google.common.util.concurrent.RateLimiter;
import java.net.SocketTimeoutException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeoutException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * 此类会在ServiceManager线程外使用（ClientSource），故其中的资源必须做好同步
 * states使用ConcurrentHashMap也是出于此需求考虑
 * @author arksea
 */
public class ServiceInstance implements Comparable {
    private final static Logger logger = LogManager.getLogger(ServiceInstance.class);
    private static final long DEF_RATE_UPGRADE_PERIOD = 15000;
    private static final double DEF_RATE_UPGRADE = 1.5;
    private static final int MULTIPLE = 10; //为了在Step很小时做流量提升更加平滑
                                            //permitsPerSecond与step在实际使用时
                                            //乘以此数进行放大
    protected final MsgSvcAddr addr;
    private MsgServiceDefine svcdef;
    protected final ConcurrentHashMap<String, Object> states = new ConcurrentHashMap();
    private RateLimiter rateLimiter;//流控类
    private int permitsPerSecond;   //每秒可申请Tocken数,小等于0表示不做客户端流控
    private int minRateLimit;       //允许限流的最小值
    private int acquireStep;        //每次调用需要申请的Tocken数
    private long rateUpgradePeriod; //限流提升周期，单位为毫秒
    private double rateUpgrade;     //限流提升倍率
    private int enableClientRateLimitMaxRT;//当客户端RT时间超过此毫秒数时开启客户端流控
    private boolean enableClientRateLimit;
    private long lastChangeStepTime;
    private long requestCount;
    private long lastRequestCount;
    private long lastQPSTime; //记录前一次计算QPS的时刻
    private int QPS;
    //服务调用失败时降低相应服务实例的流量
    //每次调用需要申请的Tocken数等于失败率乘每秒限流量乘系数，最小为1，最大为每秒限流量，
    //这意味着每秒调用次数为失败率乘以系数后的倒数，最大值为每秒限流量，
    //比如假设系数为1.0，失败率100%时每秒只允许调用一次,失败率0%时由设定的每秒限流量确定
    //失败率1%时每秒限流100次，失败率2%时每秒限流50次
    private int timeoutCount;

    public ServiceInstance(MsgSvcAddr addr) {
        this.addr = addr;
    }

    
    public synchronized void init(MsgServiceDefine def) {
        this.svcdef = def;
        permitsPerSecond = 1000;
        minRateLimit = 1;
        enableClientRateLimitMaxRT = Integer.MAX_VALUE;
        acquireStep = MULTIPLE;
        enableClientRateLimit = false;
        lastChangeStepTime = 0;
        rateUpgradePeriod = DEF_RATE_UPGRADE_PERIOD;
        rateUpgrade = DEF_RATE_UPGRADE;
        timeoutCount = 0;
        requestCount = 0;
        lastRequestCount = 0;
        lastQPSTime = System.currentTimeMillis();
        QPS = 0;
        Object obj = def.getProperty("client_rate_limit");
        if ( obj != null) {
            resetRateLimiter(obj);
        } else {
            this.rateLimiter = RateLimiter.create(permitsPerSecond*MULTIPLE);
        }
        obj = def.getProperty("client_rate_limit_rt");
        if (obj != null) {
            resetRateLimiterRT(obj);
        }
        obj = def.getProperty("rate_upgrade");
        if (obj != null) {
            resetRateUpgrade(obj);
        }
    }
    
    void setState(String name, Object value) {
        states.put(name, value);
        if (name.equals("client_rate_limit")) {
            resetRateLimiter(value);
        } else if (name.equals("client_rate_limit_rt")) {
            resetRateLimiterRT(value);
        } else if (name.equals("rate_upgrade")) {
            resetRateUpgrade(value);
        }
    }
    
    //更新限流参数
    private synchronized void resetRateLimiter(Object value) {
        try {
            String[] vl = ((String) value).split(",");
            int permits;
            int min;
            if (vl.length == 3) {
                enableClientRateLimit = vl[0].equals("enable");
                permits = Integer.parseInt(vl[1]);
                min = Integer.parseInt(vl[2]);
            } else {
                enableClientRateLimit = false;
                permits = -1;
                min = 1;                
            }
            if (permits <= 0) {
                //当没参数小等于0时，流控仅作用于出现失败或超时的调用后控制流量
                this.permitsPerSecond = 1000;
                this.minRateLimit = 1;
            } else {
                this.permitsPerSecond = permits;
                if (min <= 0) {
                    this.minRateLimit = 1;
                } else if (min > permits) {
                    this.minRateLimit = permits;
                } else {
                    this.minRateLimit = min;
                }
            }
            this.acquireStep = MULTIPLE;
        } catch (NumberFormatException ex) {
            logger.error("更新限流参数失败", ex);
        }
        //permitsPerSecond与step在实际使用时乘以10的目的是
        //为了在Step很小时做流量提升更加平滑
        this.rateLimiter = RateLimiter.create(permitsPerSecond*MULTIPLE);
    }
    
    //更新限流参数
    private synchronized void resetRateLimiterRT(Object value) {
        try {
            enableClientRateLimitMaxRT = Integer.parseInt(value.toString());
        } catch (NumberFormatException ex) {
            logger.error("更新开启限流RT门限失败", ex);
        }
    }
    
    //更新限流提升策略参数
    private synchronized void resetRateUpgrade(Object value) {
        try {
            String[] vl = ((String) value).split(",");
            double n = DEF_RATE_UPGRADE;
            long period = DEF_RATE_UPGRADE_PERIOD;
            if (vl.length == 2) {
                n = Double.parseDouble(vl[0]);
                period = Long.parseLong(vl[1]);
            } else if (vl.length == 1) {
                n = Double.parseDouble(vl[0]);
            }
            if (n <= 1.0) {
                n = DEF_RATE_UPGRADE;
            }
            if (period <= 0) {
                period = DEF_RATE_UPGRADE_PERIOD;
            }
            rateUpgrade = n;
            rateUpgradePeriod = period;
        } catch (NumberFormatException ex) {
            logger.error("更新限流提升策略参数失败", ex);
        }
    }

    public void onServiceFailed(Throwable ex) {
        //在出现超时错误时启用超时流控策略
        Throwable cause = ex.getCause();
        if (cause instanceof SocketTimeoutException || cause instanceof TimeoutException) {
            synchronized (this) {
                if (timeoutCount == 0) {
                    //第一次出现超时需要重置流量提升的衰减延时起始时间
                    lastChangeStepTime = System.currentTimeMillis();
                }
                if (++timeoutCount > permitsPerSecond) {
                    timeoutCount = permitsPerSecond;
                }
                downgradeRate();
            }
        }
    }
    
    public synchronized void onServiceSucceed(long rt) {
        //当RT时间超过预设的最大门限时启动超时流控策略
        if (rt>enableClientRateLimitMaxRT) {
            if (timeoutCount == 0) {
                //第一次出现错误需要重置流量提升的衰减延时起始时间
                lastChangeStepTime = System.currentTimeMillis();
            }
            if (++timeoutCount > permitsPerSecond) {
                timeoutCount = permitsPerSecond;
            }
            downgradeRate();
        }
    }
    
    public synchronized boolean tryAcquire() {
        if (!enableClientRateLimit && timeoutCount == 0) {
            requestSuccess();
            return true;
        } else {
            if (timeoutCount>0) {
                //流量提升策略：超时数根据上“一次衰减时间”或者“最后一次修改Step时间”延时5秒进行衰减
                long now = System.currentTimeMillis(); 
                if((now - lastChangeStepTime) > rateUpgradePeriod) {
                    lastChangeStepTime = now;
                    upgradeRate();
                }
            }
            //permitsPerSecond与step在实际使用时乘以10的目的是
            //为了在Step很小时做流量提升更加平滑
            boolean success = rateLimiter.tryAcquire(acquireStep);
            if (success) {
                requestSuccess();
            }
            return success;
        }
    }
    
    private void requestSuccess() {
        ++ requestCount;
        long now = System.currentTimeMillis();
        long t = (now - lastQPSTime) / 1000;
        if (t > 5) {
            QPS = (int) ((requestCount - lastRequestCount) / t);
            lastRequestCount = requestCount;
            lastQPSTime = now;
            if (enableClientRateLimit) {
                int r = (int)(QPS*100.0/permitsPerSecond);
                if (r > 70) {
                    String msg = "客户端QPS已经达到设定限流值的"+r+"%,QPS="+QPS+",regname="+svcdef.regname+",addr="+toString(); 
                    if (r > 80) {
                        logger.error(msg);
                    } else {
                        logger.warn(msg);
                    }
                }
            }
        }
    }
   
    public void onConnectSucceed() {
        states.put("online", "true");
    }
    
    public void onConnectFailed(Throwable exception) {
        //无法连接服务器将暂时把服务实例设置为离线状态，直到服务器主动将其设置为在线
        states.put("online", "false");
    }
    
    private void downgradeRate() {
        int step;
        int restQPS = (int)(QPS / rateUpgrade);
        if (restQPS <= 0) {
            //目标QPS最小为1
            step = permitsPerSecond * MULTIPLE;
        } else {
            step = (int)(permitsPerSecond*1.0/restQPS)*MULTIPLE;
            //step = (int)(failedCount*1.0/QPS * permitsPerSecond)*MULTIPLE;
        }
        resetAcquireStep(step, "Downgrade Rate");
    }
    
    private void upgradeRate() {
        int step = (int) (this.acquireStep / rateUpgrade);
        resetAcquireStep(step, "Upgrade Rate");
        timeoutCount = (int) (timeoutCount / rateUpgrade);
        if (this.acquireStep > MULTIPLE && timeoutCount < 1) {
            timeoutCount = 1;
        } else if (this.acquireStep == MULTIPLE) {
            timeoutCount = 0;
        }
    }
    
    private void resetAcquireStep(int step, String logMsg) {
        //最小限流控制，如果没设置的话minRateLimit的值是1，此时最小限流为1次/秒
        int maxStep = permitsPerSecond/minRateLimit*MULTIPLE;
        if (step > maxStep) {
            step = maxStep;
        } else if (step < MULTIPLE) {
            step = MULTIPLE;
        }
        if(this.acquireStep != step) {
            logger.warn(logMsg+": Step = "+step+", failedCount="+timeoutCount+", QPS="+QPS);
            lastChangeStepTime = System.currentTimeMillis();
            this.acquireStep = step;
        }
    }
    
    public MsgSvcAddr getAddr() {
        return addr;
    }

    public Object getState(String name) {
        return states.get(name);
    }
    
    @Override
    public String toString() {
        return addr.toString() + " " + states;
    }
    
    @Override
    public int compareTo(Object o) {
        ServiceInstance other = (ServiceInstance) o;
        return this.addr.compareTo(other.addr);
    }
    
    public static void main(String[] args) {
        ServiceInstance i = new ServiceInstance(new MsgSvcAddr("127.0.0.1", 9090));
        i.setState("online", "true");
        i.setState("failed", "false");
        JsonCodec codec = new JsonCodec();
        String str = codec.encodec("instance", i);
        System.out.println(str);
    }
}
