namespace com.baidu.dsf.server
{

    using TServer = Thrift.Server.TServer;

    /// 
    /// <summary>
    /// @author Administrator
    /// </summary>
    public interface IThriftServerFactory
    {
        TServer create();
    }

}