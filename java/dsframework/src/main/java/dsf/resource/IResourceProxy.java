package dsf.resource;

/**
 * <p>Title: 资源代理接口</p>
 *
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company: </p>
 *
 * @author not attributable
 * @version $Revision: 1.1.5.1 $
 */
public interface IResourceProxy extends IDisposable {

    String getResourceClassName();

    String getDisposeMethod();
}
