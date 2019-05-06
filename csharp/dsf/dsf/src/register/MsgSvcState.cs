namespace com.baidu.dsf.register
{

    /// <summary>
    /// 服务状态消息
    /// {
    ///  "name" : "online",      //状态名
    ///  "value":"true"          //状态值
    /// }
    /// @author arksea
    /// </summary>
    public class MsgSvcState
    {

        public readonly string name;
        public readonly string value;

        public MsgSvcState(string name, string value)
        {
            this.name = name;
            this.value = value;
        }
    }

}