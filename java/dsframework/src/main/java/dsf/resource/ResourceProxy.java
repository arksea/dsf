package dsf.resource;

import java.lang.reflect.*;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * <p>Title:资源代理类</p>
 *
 * <p>Description: 代理没有实现IDisposeable接口的资源</p>
 *
 * <p>Copyright: Copyright (c) 2013</p>
 *
 * <p>Company: 91.baidu.com</p>
 *
 * @author $Author: xiaohaixing_298852 $
 * @version $Revision: 1.1.5.1 $
 */
public class ResourceProxy implements InvocationHandler, IResourceProxy {

    private volatile boolean disposed;
    private final Object resource;
    private String disposeMethod;
    private final static Logger logger = LogManager.getLogger(ResourceProxy.class.getName());

    private ResourceProxy(Object resource, String method) {
        this.resource = resource;
        this.disposeMethod = method;
        this.disposed = false;
    }

    public String getDisposeMethod() {
        return disposeMethod;
    }

    public String getResourceClassName() {
        return resource.getClass().getName();
    }

    public static Object newInstance(Object resource, String disposeMethod, Class interfac) {
        try {
            Class proxyClass = Proxy.getProxyClass(
                    Thread.currentThread().getContextClassLoader(),
                    new Class[]{interfac, IResourceProxy.class});
            Constructor con = proxyClass.getConstructor(
                    new Class[]{InvocationHandler.class});
            return con.newInstance(new Object[]{new ResourceProxy(resource, disposeMethod)});
        } catch (Exception e) //catch (InvocationTargetException ex)
        //catch (IllegalAccessException ex)
        //catch (InstantiationException ex)
        //catch (SecurityException ex)
        //catch (NoSuchMethodException ex)
        //catch (IllegalArgumentException ex)
        {
            throw new RuntimeException("创建资源代理类失败", e);
        }
    }

    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        String methodName = method.getName();
        //以下调用代理类的方法
        if (methodName.equals("isDisposed")) {
            return this.isDisposed();
        } else if (methodName.equals("getResourceClassName")) {
            return this.getResourceClassName();
        } else if (methodName.equals("getDisposeMethod")) {
            return this.getDisposeMethod();
        } else if (methodName.equals("dispose")) {
            return method.invoke(this, args);
        }
        //以下调用实际的资源对象的方法
        if (methodName.equals(disposeMethod)) {   //在执行指定的清理方法前先将disposed设置为true
            this.disposed = true;
        }
        return method.invoke(resource, args);
    }

    public boolean isDisposed() {
        return this.disposed;
    }

    public void dispose() {
        try {
            Method method = resource.getClass().getMethod(disposeMethod);
            method.invoke(resource);
        } catch (Exception ex) {
            logger.error("dispose failed", ex);
        }
    }
}
