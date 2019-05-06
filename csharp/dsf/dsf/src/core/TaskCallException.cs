using System;

namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class TaskCallException : Exception
    {

        public TaskCallException(string name, Exception ex) : base(name, ex)
        {
        }

        public TaskCallException(string name) : base(name)
        {
        }
    }

}