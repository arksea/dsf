package dsf.register;

import dsf.core.Message;
import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * @author arksea
 */
public class SocketWritterSync implements ISocketWritter {

    private final static Logger logger = LogManager.getLogger(SocketWritterSync.class.getName());
    private final Socket channel;
    private final int BLOCK_SIZE = 4096;
    private final JsonCodec codec = new JsonCodec();

    public SocketWritterSync(Socket channel) {
        this.channel = channel;
    }

    @Override
    public void send(Message msg) throws IOException{
        send(msg.name, msg.value);
    }

    @Override
    public void send(String name, Object value) throws IOException {
        String line = codec.encodec(name, value);
        logger.debug("socke send line: " + line);
        byte[] bytes = line.getBytes();
        int len;
        OutputStream out = channel.getOutputStream();
        for (int i = 0; i < bytes.length; i += BLOCK_SIZE) {
            if ((i + BLOCK_SIZE) < bytes.length) {
                len = BLOCK_SIZE;
                out.write(bytes, i, len);
                out.flush();
            } else {
                len = bytes.length - i;
                out.write(bytes,i,len);
                out.write(10);
                out.flush();
            }
        }
    }

}
