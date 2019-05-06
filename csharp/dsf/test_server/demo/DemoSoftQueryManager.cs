namespace com.baidu.dsf.demo.server
{

    using com.baidu.dsf.server;
    using baidu91.appsvc.demo;

    /// 
    /// <summary>
    /// @author xhx
    /// </summary>
    public class DemoSoftQueryManager : ThriftServiceManager<DemoSoftQuery.Client>
    {
        public virtual void delayRequestOnce(long rt)
        {
            DemoSoftQueryHandler h = (DemoSoftQueryHandler)serviceHandler;
            h.delayRequestOnce(rt);
        }
    }

}