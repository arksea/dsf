using System;

namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class ThrowMessage : Message
    {
        //希望调用者抛出错误异常

        public ThrowMessage(string error) : base("__THROW", error)
        {
        }
        //希望调用者将传入的异常作为cause一起抛出
        //此异常通常是因为被调用线程中发生了意料外错误而崩溃

        public ThrowMessage(Exception ex) : base("__THROW", ex)
        {
        }
    }

}