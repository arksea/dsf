package dsf.resource;

/**
 * 用于显式释放资源的接口
 * @author arksea
 */
public interface IDisposable {

    boolean isDisposed();

    void dispose();
}
