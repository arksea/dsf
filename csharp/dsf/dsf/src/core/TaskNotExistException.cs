using System;

namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class TaskNotExistException : Exception
    {

        public TaskNotExistException(string name) : base(name)
        {
        }

        public TaskNotExistException(string name, Exception ex) : base(name, ex)
        {
        }
    }

}