package dsf.core;

/**
 *
 * @author arksea
 */
public class TaskCallException extends RuntimeException {

    public TaskCallException(String name, Throwable ex) {
        super(name, ex);
    }

    public TaskCallException(String name) {
        super(name);
    }
}
