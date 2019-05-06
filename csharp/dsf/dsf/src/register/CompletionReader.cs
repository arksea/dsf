using System;

namespace com.baidu.dsf.register
{

    using com.baidu.dsf.core;
    using java.nio;
    using java.nio.channels;

    /// <summary>
    /// Socket异步读取
    /// @author xhx
    /// </summary>
    public class CompletionReader
    {

        public CompletionReader(AsynchronousSocketChannel channel, MessageChildTask consumer)
        {
            bytes = new byte[ASSIGN_SIZE * 2];
            len = 0;
            next = 0;
            offset = 0;
            byteBuffer = ByteBuffer.wrap(bytes,0,ASSIGN_SIZE);
            this.channel = channel;
            this.consumer = consumer;
            readHandler = new CompletionHandlerAnonymousInnerClassHelper(this);
            request();
        }

        private readonly AsynchronousSocketChannel channel;
        private MessageChildTask consumer;
        private readonly int ASSIGN_SIZE = 512;
        private ByteBuffer byteBuffer;
        private byte[] bytes;
        private int len; //已缓存未读取数据的长度
        private int next; //已缓存未读取数据的起始位置
        private int offset; //当前扫描到的位置(搜索\n)

        private void request()
        {
            assign();
            channel.read(byteBuffer, true, readHandler);
        }

        private void preCompleted()
        {
            byteBuffer.flip();
            len = byteBuffer.limit() - next;
        }

        private void postCompleted()
        {
            request();
        }

        private void sendToConsumer(Message msg)
        {
            try
            {
                consumer.putInfo(msg);
            }
            catch (Exception e)
            {
                DSFLogHelper.ErrorLog("CompletionReader",e, "send to consumer failed, " + msg);
            }
        }
        private void assign()
        {
            if (len == 0)
            {
                next = 0;
                offset = 0;
            }
            else if (next + len + ASSIGN_SIZE > bytes.Length)
            {
                if (len <= next)
                {
                    Array.Copy(bytes, next, bytes, 0, len);
                    offset -= next;
                    next = 0;
                }
                else
                {
                    int newSize = bytes.Length;
                    while (len + ASSIGN_SIZE >= newSize)
                    {
                        newSize *= 2;
                    }
                    byte[] b = new byte[newSize];
                    Array.Copy(bytes, next, b, 0, len);
                    bytes = b;
                    offset -= next;
                    next = 0;
                }
            }
            byteBuffer = ByteBuffer.wrap(bytes, next + len, ASSIGN_SIZE);
        }

        private string readLine()
        {
            do
            {
                if (offset >= next + len)
                {
                    return null;
                }
            } while (bytes[offset++] != '\n');
            int n = offset - next - 1;
            string str = System.Text.Encoding.UTF8.GetString(bytes, next, n);
            next = offset;
            len = len - n - 1;
            return str;
        }

        private CompletionHandler readHandler;

        private class CompletionHandlerAnonymousInnerClassHelper : CompletionHandler
        {
            public CompletionHandlerAnonymousInnerClassHelper(CompletionReader o)
            {
                this.outerInstance = o;
            }
            private CompletionReader outerInstance;
            private JsonCodec codec = new JsonCodec();
            public void completed(object result, object attachment)
            {
                if (((java.lang.Integer)result).intValue() == -1)
                {
                    outerInstance.consumer.putInfo(new Message("channel_error", "peer is closed socket"));
                }
                else
                {
                    string line = null;
                    try
                    {
                        outerInstance.preCompleted();
                        do
                        {
                            line = outerInstance.readLine();
                            if (line != null && !line.Trim().Equals(""))
                            {
                                Message msg = codec.decodec(line);
                                DSFLogHelper.DebugLog("CompletionReader", "socke recv line: " + line);
                                outerInstance.sendToConsumer(new Message("tcp", msg));
                            }
                        } while (line != null);
                        outerInstance.postCompleted();
                    }
                    catch (Exception ex)
                    {
                        DSFLogHelper.ErrorLog("CompletionReader", ex, "channel_error: " + line);
                        outerInstance.sendToConsumer(new Message("channel_error", ex.GetType().FullName + ":" + ex.Message + "(" + line + ")"));
                    }
                }
            }
            public void failed(Exception exc, object attachment)
            {
                outerInstance.sendToConsumer(new Message("channel_error", "read socket channel failed: " + exc.Message));
            }
        }
    }

}