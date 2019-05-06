package dsf.core;

/**
 * 匹配指定名字的消息
 * @author arksea
 */
public class MsgMatcher implements IMessageMatcher {

    private final String name;

    public MsgMatcher(String name) {
        this.name = name;
    }

    @Override
    public Message match(Message msg) throws Throwable {
        return msg.name.equals(name) ? msg : null;
    }
}
