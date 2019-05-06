namespace com.baidu.dsf.pool
{

    /// <summary>
    /// @author arksea
    /// </summary>
    public interface IClientSource<I>
    {

        I getClient();

        void returnClient(object p); //归还一个客户端

        string getServiceRegname();
    }

}