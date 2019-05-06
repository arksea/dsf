package dsf.register;

import dsf.core.Message;
import dsf.core.MessageChildTask;
import java.io.IOException;
import java.net.Socket;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Socket读取
 * @author xhx
 */
public class SocketReaderSync {

    public SocketReaderSync(Socket channel, MessageChildTask consumer) {
        bytes = new byte[ASSIGN_SIZE*2];
        len = 0;
        next = 0;
        offset = 0;
        this.channel = channel;
        this.consumer = consumer;
        Thread t = new Thread(readHandler);
        t.setName("RegisterClient Socket Reader");
        t.setDaemon(true);
        t.start();
    }
    private final static Logger logger = LogManager.getLogger(SocketReaderSync.class.getName());
    private final Socket channel;
    private MessageChildTask consumer;
    private final int ASSIGN_SIZE = 512;
    private byte[] bytes;
    private int len;    //已缓存未读取数据的长度
    private int next;   //已缓存未读取数据的起始位置
    private int offset; //当前扫描到的位置(搜索\n)
    
    private int request() throws IOException {
        assign();
        int limit = next+len;//下一个写入位置
        int n = channel.getInputStream().read(bytes, limit, bytes.length - limit);
        if (n >= 0) {
            len += n;
        }
        return n;
    }

    private void sendToConsumer(Message msg) {
        try {
            consumer.putInfo(msg);
        } catch (Exception e) {
            logger.error("send to consumer failed, " + msg, e);
        }
    }
    private void assign() {
        if (len == 0) {
            next = 0;
            offset = 0;
        } else if (next+len+ASSIGN_SIZE > bytes.length) {
            if (len <= next) {
                System.arraycopy(bytes, next, bytes, 0, len);
                offset -= next;
                next = 0;
            } else {
                int newSize = bytes.length;
                while (len+ASSIGN_SIZE>=newSize) {
                    newSize*=2;
                }
                byte[] b = new byte[newSize];
                System.arraycopy(bytes, next, b, 0, len);
                bytes = b;
                offset -= next;
                next = 0;
            }
        }
    }

    private String readLine() {
        do {
            if (offset >= next+len) {
                return null;
            }
        } while(bytes[offset++] != '\n');
        int n = offset-next-1;
        String str = new String(bytes, next, n);
        next = offset;
        len  = len - n - 1;
        return str;
    }

    private final Runnable readHandler = new Runnable() {
        private JsonCodec codec = new JsonCodec();

        @Override
        public void run() {
            try {
                while (true) {
                    int result = request();
                    if (result == -1) {
                        if (!channel.isClosed()) {
                            consumer.putInfo(new Message("channel_error", "peer is closed socket"));
                        }
                        break;
                    } else {
                        String line = null;
                        do {
                            line = readLine();
                            if (line != null && !line.trim().equals("")) {
                                Message msg = codec.decodec(line);
                                logger.debug("socke recv line: " + line);
                                sendToConsumer(new Message("tcp", msg));
                            }
                        } while (line != null);
                    }
                }
            }
            catch (Throwable ex) {
                sendToConsumer(new Message("channel_error", "read socket channel failed, " +ex.getClass().getName()+": "+ ex.getMessage()));
            }
        }
    };
}
