package dsf.core;

/**
 *
 * @author arksea
 */
public class Message {

    public final String name;
    public final Object value;

    public Message(String name, Object value) {
        this.name = name;
        this.value = value;
    }

    @Override
    public String toString() {
        return name + ":" + value;
    }
}
