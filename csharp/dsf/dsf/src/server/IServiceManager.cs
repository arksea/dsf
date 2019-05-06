namespace com.baidu.dsf.server
{

    /// 
    /// <summary>
    /// @author xhx
    /// </summary>
    public interface IServiceManager
    {
        void onAppStart();
        void afterAppStart();
        void onAppStop();
        void onDialed();
        void onStat();
        void setManagePort(int managePort);
    }

}