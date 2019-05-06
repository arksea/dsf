package dsf.core;

/**
 *
 * @author arksea
 */
public class TaskNotExistException extends RuntimeException {

    public TaskNotExistException(String name) {
        super(name);
    }

    public TaskNotExistException(String name, Throwable ex) {
        super(name, ex);
    }
}
