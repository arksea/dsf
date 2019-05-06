package dsf.pool;

/**
 * @author arksea
 */
public interface IClientSource<I> {
    public I getClient();   //获取一个客户端接口
    public void returnClient(I p); //归还一个客户端
}
