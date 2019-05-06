package dsf.core;

/**
 *
 * @author arksea
 */
public class ThrowMessage extends Message {
    //希望调用者抛出错误异常

    public ThrowMessage(String error) {
        super("__THROW", error);
    }
    //希望调用者将传入的异常作为cause一起抛出
    //此异常通常是因为被调用线程中发生了意料外错误而崩溃

    public ThrowMessage(Throwable ex) {
        super("__THROW", ex);
    }
}
