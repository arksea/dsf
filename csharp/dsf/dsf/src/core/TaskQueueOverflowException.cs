using System;

namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class TaskQueueOverflowException : Exception
    {

        public TaskQueueOverflowException(string name) : base(name)
        {
        }

        public TaskQueueOverflowException(string name, Exception ex) : base(name, ex)
        {
        }
    }

}