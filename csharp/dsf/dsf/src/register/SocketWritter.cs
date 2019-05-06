namespace com.baidu.dsf.register
{

    using com.baidu.dsf.core;
    using java.nio;
    using java.nio.channels;

    /// <summary>
    /// @author arksea
    /// </summary>
    public class SocketWritter
    {

        private readonly AsynchronousSocketChannel channel;
        private readonly ByteBuffer writeByteBuffer;
        private readonly int BUFFER_SIZE = 4224;
        private readonly int BLOCK_SIZE = 4096;
        private readonly JsonCodec codec = new JsonCodec();

        public SocketWritter(AsynchronousSocketChannel channel)
        {
            this.channel = channel;
            writeByteBuffer = ByteBuffer.allocate(BUFFER_SIZE);
        }

        public virtual void send(Message msg)
        {
            send(msg.name, msg.value);
        }

        public virtual void send(string name, object value)
        {
            string line = codec.encodec(name, value);
            DSFLogHelper.DebugLog("SocketWritter", "socke send line: " + line);
            byte[] bytes = System.Text.Encoding.UTF8.GetBytes(line);
            int len;
            for (int i = 0; i < bytes.Length; i += BLOCK_SIZE)
            {
                if ((i + BLOCK_SIZE) < bytes.Length)
                {
                    len = BLOCK_SIZE;
                    writeByteBuffer.clear();
                    writeByteBuffer.put(bytes, i, len);
                    writeByteBuffer.flip();
                    channel.write(writeByteBuffer).get();
                }
                else
                {
                    len = bytes.Length - i;
                    writeByteBuffer.clear();
                    writeByteBuffer.put(bytes, i, len);
                    writeByteBuffer.put((byte) 10); // "\n"
                    writeByteBuffer.flip();
                    channel.write(writeByteBuffer).get();
                }
            }
        }

    }

}