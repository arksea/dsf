package com.baidu.dsf.register;

import dsf.core.*;
import dsf.register.ISocketWritter;
import dsf.register.JsonCodec;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.concurrent.ExecutionException;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

/**
 * @author arksea
 */
public class SocketWritter implements ISocketWritter {

    private final static Logger logger = LogManager.getLogger(SocketWritter.class.getName());
    private final AsynchronousSocketChannel channel;
    private final ByteBuffer writeByteBuffer;
    private final int BUFFER_SIZE = 4224;
    private final int BLOCK_SIZE = 4096;
    private final JsonCodec codec = new JsonCodec();

    public SocketWritter(AsynchronousSocketChannel channel) {
        this.channel = channel;
        writeByteBuffer = ByteBuffer.allocate(BUFFER_SIZE);
    }

    @Override
    public void send(Message msg) throws InterruptedException, ExecutionException {
        send(msg.name, msg.value);
    }

    @Override
    public void send(String name, Object value) throws InterruptedException, ExecutionException {
        String line = codec.encodec(name, value);
        logger.debug("socke send line: " + line);
        byte[] bytes = line.getBytes();
        int len;
        for (int i = 0; i < bytes.length; i += BLOCK_SIZE) {
            if ((i + BLOCK_SIZE) < bytes.length) {
                len = BLOCK_SIZE;
                writeByteBuffer.clear();
                writeByteBuffer.put(bytes, i, len);
                writeByteBuffer.flip();
                channel.write(writeByteBuffer).get();
            } else {
                len = bytes.length - i;
                writeByteBuffer.clear();
                writeByteBuffer.put(bytes, i, len);
                writeByteBuffer.put((byte) 10); // "\n"
                writeByteBuffer.flip();
                channel.write(writeByteBuffer).get();
            }
        }
    }

}
