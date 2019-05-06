package dsf;

/**
 *
 * @author arksea
 */
public class DSFrameworkException extends Exception {

    public DSFrameworkException(String msg) {
        super(msg);
    }

    public DSFrameworkException(String msg, Throwable ex) {
        super(msg, ex);
    }
}
