package dsf.route;

/**
 *
 * @author arksea
 */
public class RateLimitedException extends RuntimeException {

    public RateLimitedException(String msg) {
        super(msg);
    }

    public RateLimitedException(String msg, Throwable ex) {
        super(msg, ex);
    }
}
