package dsf.core;

/**
 *
 * @author arksea
 */
public class Result {

    public Message msg = null;
    //ChildTask.handle_call返回后将设置此标记为true
    //TaskContext.call方法使用此标记判断调用是否已完成
    //但没有完成将抛出一个超时异常
    public boolean resulted = false;
}
