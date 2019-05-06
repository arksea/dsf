package dsf.core;

/**
 *
 * @author arksea
 */
public class TaskQueueOverflowException extends RuntimeException {

    public TaskQueueOverflowException(String name) {
        super(name);
    }

    public TaskQueueOverflowException(String name, Throwable ex) {
        super(name, ex);
    }
}
