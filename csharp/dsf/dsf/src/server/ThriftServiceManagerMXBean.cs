using System;
namespace com.baidu.dsf.server
{
    public interface ThriftServiceManagerMXBean
    {
        bool getAutoRegisterAfterAppStart();
        string getDialedMethod();
        long getMaxRetryDelay();
        int getMaxThreadPoolThreads();
        long getMinRetryDelay();
        int getMinThreadPoolThreads();
        long getQPS();
        string getRegname();
        long getRequestCount();
        string getServiceBindHost();
        int getServiceBindPort();
        int getManagePort();
        string getServiceName();
        string getVersion();
        bool isRegistered();
        void registerService();
        void setAutoRegisterAfterAppStart(bool value);
        void setMaxThreadPoolThreads(int value);
        void setMinRetryDelay(long value);
        void setMinThreadPoolThreads(int value);
        void setServiceBindHost(string value);
        void setServiceBindPort(int value);
        void setServiceName(string value);
        void setVersion(string value);
        void startService();
        string stopService();
        void unregisterService();
    }
}
