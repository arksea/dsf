package dsf.register;

import com.google.gson.Gson;
import java.util.Map;

/**
 * 服务定义消息
 * {
 *  "regname":"service1 ver1.0",   //服务注册名,通常包含路径、服务名与版本信息
 *  "name": "com.xxx.Class1",      //服务名，通常使用类名，以便于框架定位并创建其实例
 *  "version":"1.0",               //服务版本号
 *  "props":
 *   {
 *      "protocol":"Thrift.binary"     //协议类型
 *      "route_strategy":"roundrobin"   //路由策略
 *      "timeout":"3000",
 *      "fail_strategy":"xxx"           // 失败转移策略
 *   }
 * }
 * @author arksea
 */
public class MsgServiceDefine {

    public final String regname;
    public final String name;
    public final String version;
    String description;
    Map<String, String> props;

    public MsgServiceDefine(String regname, String name, String version, String description, Map<String, String> props) {
        this.regname = regname;
        this.name = name;
        this.version = version;
        this.props = props;
        this.description = description;
    }

    public void update(MsgServiceDefine o) {
        //regname、name与version不允许修改
        description = o.description;
        props.clear();
        props.putAll(o.props);
    }
    
    public String getProperty(String name) {
        return props.get(name);
    }

    public String getDescription() {
        return description;
    }

    @Override
    public String toString() {
       return new Gson().toJson(this);
    }
}
