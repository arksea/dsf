package dsf.route;

/**
 *
 * @author xiaohaixing
 */
public class NoUseableServiceException extends RuntimeException {

    public NoUseableServiceException(String msg) {
        super(msg);
    }

    public NoUseableServiceException(String msg, Throwable ex) {
        super(msg, ex);
    }
}
