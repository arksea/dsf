package dsf.core;

/**
 * 匹配列表中任一名字的消息
 * @author arksea
 */
public class MsgsMatcher implements IMessageMatcher {

    private final String[] names;

    public MsgsMatcher(String[] names) {
        this.names = names;
    }

    @Override
    public Message match(Message msg) throws Throwable {
        for (String name : names) {
            if (msg.name.equals(name)) {
                return msg;
            }
        }
        return null;
    }
}
