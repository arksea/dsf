namespace com.baidu.dsf.core
{

    /// <summary>
    /// 匹配列表中任一名字的消息
    /// @author arksea
    /// </summary>
    public class MsgsMatcher : IMessageMatcher
    {

        private readonly string[] names;

        public MsgsMatcher(string[] names)
        {
            this.names = names;
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public Message match(Message msg) throws Throwable
        public virtual Message match(Message msg)
        {
            foreach (string name in names)
            {
                if (msg.name.Equals(name))
                {
                    return msg;
                }
            }
            return null;
        }
    }

}