namespace com.baidu.dsf.core
{

    /// <summary>
    /// 匹配二级嵌套的消息
    /// @author arksea
    /// </summary>
    public class NamedMsgsMatcher : IMessageMatcher
    {

        private readonly string name;
        private readonly string[] namedMsgs;

        public NamedMsgsMatcher(string name, string[] namedMsgs)
        {
            this.name = name;
            this.namedMsgs = namedMsgs;
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public Message match(Message msg) throws Throwable
        public virtual Message match(Message msg)
        {
            if (!msg.name.Equals(name))
            {
                return null;
            }
            Message namedMsg = (Message) msg.value;
            foreach (string str in namedMsgs)
            {
                if (namedMsg.name.Equals(str))
                {
                    return namedMsg;
                }
            }
            return null;
        }
    }

}