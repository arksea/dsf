namespace com.baidu.dsf.register
{

    /// <summary>
    /// 通讯消息名常量定义
    /// @author arksea
    /// </summary>
    public class MSG_NAMES
    {
        //tcp消息,内部消息
        //tcp=tcpmsg
        //SocketChannel接收到并send给RegisterClient的内部消息

        public const string MSG_TCP = "tcp";
        //连接失败消息,内部消息
        //SocketChannel出错时send给RegisterClient的内部消息
        public const string MSG_CHANNEL_ERROR = "channel_error";
        //心跳与应答消息
        //tick=ID
        //tack=ID
        public const string MSG_TICK = "tick";
        public const string MSG_TACK = "tack";
        //同步调用时的错误返回消息
        //error_result="error info"
        public const string MSG_ERROR_RESULT = "error_result";
        public const string MSG_ERROR = "error";
        //同步调用成功或失败的返回消息，用于无需回传其他参数，但需要等待结果的调用
        //succeed="true" 或 succeed="false"
        public const string MSG_SUCCEED = "succeed";
        //登录请求与返回消息，同步调用，传递"订阅号"参数，
        //如果订阅号已失效将返回新订阅号，否则将收到的订阅号直接返回
        //login="subscribe_id"
        //login_succeed="new_subscribe_id"
        public const string MSG_LOGIN = "login";
        public const string MSG_SYNC_SUBLIST = "sync_sublist";
        public const string MSG_UPDATE_SUBLIST = "update_sublist";
        public const string MSG_LOGIN_SUCCEED = "login_succeed";
        //注册与注销服务
        //regsvc="service1_regname"
        //unregsvc="service1_regname"
        public const string MSG_REGSVC = "regsvc";
        public const string MSG_UNREGSVC = "unregsvc";
        //服务订阅与订阅返回消息，同步调用
        //subscribe="service1_regname"
        //subscribe_result=MsgSubscribeResult json encoded string
        public const string MSG_SUBSCRIBE = "subscribe";
        public const string MSG_SUBSCRIBE_RESULT = "subscribe_result";
        //服务状态通知消息，由注册服务器向客户端异步推送
        //notify_svc_state=MsgNotifySvcState json encoded string
        public const string MSG_NOTIFY_SVC_STATE = "notify_svc_state";
        public const string MSG_NOTIFY_SVCDEF_UPDATE = "notify_svcdef_update";
        //服务状态报告消息，由应用服务发送到注册服务器，注册服务器会用MSG_NOTIFY_SVC_STATE消息分发到各个订阅者,异步消息
        //report_svc_state=MsgReportSvcState json encoded string
        public const string MSG_REPORT_SVC_STATE = "report_svc_state";
        //服务状态设置消息，由应用服务发送到注册服务器，注册服务器不会进行分发，同步消息
        public const string MSG_SET_SVC_STATE = "set_svc_state";
        //服务定义查询请求
        public const string MSG_QUERY_SVCDEF = "query_svcdef";
        public const string MSG_QUERY_SVCDEF_RESULT = "query_svcdef_result";
        public const string MSG_QUERY_SVC_STATE = "query_svc_state";
        public const string MSG_QUERY_SVC_STATE_RESULT = "query_svc_state_result";
        // 服务添加请求
        public const string MSG_ADD_SVCDEF = "add_svcdef";
        public const string MSG_UPDATE_SVCDEF = "update_svcdef";
        public const string MSG_UPDATE_SVCDEF_IDL = "update_svcdef_idl";
        public const string MSG_DEL_SVCDEF = "del_svcdef";
    }

}