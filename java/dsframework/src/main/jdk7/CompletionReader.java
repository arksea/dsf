package com.baidu.dsf.register;

import dsf.core.Message;
import dsf.core.MessageChildTask;
import dsf.register.JsonCodec;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.nio.channels.CompletionHandler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * Socket异步读取
 * @author xhx
 */
public class CompletionReader {

    public CompletionReader(AsynchronousSocketChannel channel, MessageChildTask consumer) {
        bytes = new byte[ASSIGN_SIZE*2];
        len = 0;
        next = 0;
        offset = 0;
        byteBuffer = ByteBuffer.wrap(bytes,0,ASSIGN_SIZE);
        this.channel = channel;
        this.consumer = consumer;
        request();
    }
    private final static Logger logger = LogManager.getLogger(CompletionReader.class.getName());
    private final AsynchronousSocketChannel channel;
    private final MessageChildTask consumer;
    private final int ASSIGN_SIZE = 512;
    private ByteBuffer byteBuffer;
    private byte[] bytes;
    private int len;    //已缓存未读取数据的长度
    private int next;   //已缓存未读取数据的起始位置
    private int offset; //当前扫描到的位置(搜索\n)
    
    private void request() {
        assign();
        channel.read(byteBuffer, true, readHandler);
    }

    private void preCompleted() {
        byteBuffer.flip();
        len = byteBuffer.limit() - next;
    }

    private void postCompleted() {
        request();
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
        byteBuffer = ByteBuffer.wrap(bytes, next+len, ASSIGN_SIZE);
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

    private final CompletionHandler<Integer, Boolean> readHandler = new CompletionHandler<Integer, Boolean>() {
        private final JsonCodec codec = new JsonCodec();
        @Override
        public void completed(Integer result, Boolean attachment) {
            if (result == -1) {
                consumer.putInfo(new Message("channel_error", "peer is closed socket"));
            } else {
                String line = null;
                try {
                    preCompleted();
                    do {
                        line = readLine();
                        if (line != null && !line.trim().equals("")) {
                            Message msg = codec.decodec(line);
                            logger.debug("socke recv line: " + line);
                            sendToConsumer(new Message("tcp", msg));
                        }
                    } while (line != null);
                    postCompleted();
                } catch (Throwable ex) {
                    logger.error("channel_error: " + line, ex);
                    sendToConsumer(new Message("channel_error", ex.getClass().getName() + ":" + ex.getMessage() + "(" + line + ")"));
                }
            }
        }
        @Override
        public void failed(Throwable exc, Boolean attachment) {
            sendToConsumer(new Message("channel_error", "read socket channel failed: " + exc.getMessage()));
        }
    };
}
