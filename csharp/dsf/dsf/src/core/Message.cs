namespace com.baidu.dsf.core
{

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class Message
    {

        public readonly string name;
        public readonly object value;

        public Message(string name, object value)
        {
            this.name = name;
            this.value = value;
        }

        public override string ToString()
        {
            return name + ":" + value;
        }
    }

}