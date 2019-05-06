package dsf.core;

/**
 * 匹配二级嵌套的消息
 * @author arksea
 */
public class NamedMsgsMatcher implements IMessageMatcher {

    private final String name;
    private final String[] namedMsgs;

    public NamedMsgsMatcher(String name, String[] namedMsgs) {
        this.name = name;
        this.namedMsgs = namedMsgs;
    }

    @Override
    public Message match(Message msg) throws Throwable {
        if (!msg.name.equals(name)) {
            return null;
        }
        Message namedMsg = (Message) msg.value;
        for (String str : namedMsgs) {
            if (namedMsg.name.equals(str)) {
                return namedMsg;
            }
        }
        return null;
    }
}
