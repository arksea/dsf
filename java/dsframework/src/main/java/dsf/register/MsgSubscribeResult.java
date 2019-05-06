package dsf.register;

import java.util.Collection;

/**
 * 订阅请求返回消息
 * {
 *  "svcdef":  //服务定义信息，请参考MsgServiceDefine
 *      {
 *          "regname":"service1 ver1.0", 
 *          "name": "com.xxx.Class1",
 *          "version":"1.0",
 *          "protocol":"Thrift.binary",
 *          "route_strategy":"roundrobin",
 *      }
 *   "svclist":
 *      [
 *          {"addr":{"host":"192.168.0.1","port":9090},"states":[{"name":"online","value":"true"}]},
 *          {"addr":{"host":"192.168.0.2","port":9090},"states":[{"name":"online","value":"false"}]}
 *      ]
 * }
 * @author arksea
 */
public class MsgSubscribeResult {

    public final MsgServiceDefine svcdef;
    public final Collection<ServiceInstance> svclist;

    public MsgSubscribeResult(MsgServiceDefine svcdef, Collection<ServiceInstance> svclist) {
        this.svcdef = svcdef;
        this.svclist = svclist;
    }
}
