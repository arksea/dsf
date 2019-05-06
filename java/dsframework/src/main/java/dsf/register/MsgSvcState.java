package dsf.register;

/**
 * 服务状态消息
 * {
 *  "name" : "online",      //状态名
 *  "value":"true"          //状态值
 * }
 * @author arksea
 */
public class MsgSvcState {

    public final String name;
    public final String value;

    public MsgSvcState(String name, String value) {
        this.name = name;
        this.value = value;
    }
}
