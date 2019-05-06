using System;
namespace com.baidu.dsf.server
{
    public interface AppManagerMXBean
    {
        string getAppName();
        string getRegisterServers();
        string stopApplication();
    }
}
