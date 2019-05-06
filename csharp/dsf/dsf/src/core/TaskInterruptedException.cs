using System;

namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class TaskInterruptedException : Exception
    {

        public TaskInterruptedException(string name, Exception ex) : base(name, ex)
        {
        }

        public TaskInterruptedException(string name) : base(name)
        {
        }
    }

}