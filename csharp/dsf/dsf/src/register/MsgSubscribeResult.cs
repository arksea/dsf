using System.Collections.Generic;

namespace com.baidu.dsf.register
{

    /// <summary>
    /// 订阅请求返回消息
    /// {
    ///  "svcdef":  //服务定义信息，请参考MsgServiceDefine
    ///      {
    ///          "regname":"service1 ver1.0", 
    ///          "name": "com.xxx.Class1",
    ///          "version":"1.0",
    ///          "protocol":"Thrift.binary",
    ///          "route_strategy":"roundrobin",
    ///      }
    ///   "svclist":
    ///      [
    ///          {"addr":{"host":"192.168.0.1","port":9090},"states":[{"name":"online","value":"true"}]},
    ///          {"addr":{"host":"192.168.0.2","port":9090},"states":[{"name":"online","value":"false"}]}
    ///      ]
    /// }
    /// @author arksea
    /// </summary>
    public class MsgSubscribeResult
    {

        public readonly MsgServiceDefine svcdef;
        public readonly ICollection<ServiceInstance> svclist;

        public MsgSubscribeResult(MsgServiceDefine svcdef, ICollection<ServiceInstance> svclist)
        {
            this.svcdef = svcdef;
            this.svclist = svclist;
        }
    }

}