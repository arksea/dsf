namespace com.baidu.dsf.core
{

    /// <summary>
    /// 匹配指定名字的消息
    /// @author arksea
    /// </summary>
    public class MsgMatcher : IMessageMatcher
    {

        private readonly string name;

        public MsgMatcher(string name)
        {
            this.name = name;
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public Message match(Message msg) throws Throwable
        public virtual Message match(Message msg)
        {
            return msg.name.Equals(name) ? msg : null;
        }
    }

}