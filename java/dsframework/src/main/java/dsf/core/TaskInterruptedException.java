package dsf.core;

/**
 *
 * @author arksea
 */
public class TaskInterruptedException extends RuntimeException {

    public TaskInterruptedException(String name, Throwable ex) {
        super(name, ex);
    }

    public TaskInterruptedException(String name) {
        super(name);
    }
}
