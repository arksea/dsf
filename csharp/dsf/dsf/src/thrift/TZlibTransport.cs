using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Thrift.Transport;

namespace com.baidu.dsf.thrift
{
    public class TZlibTransport : TTransport, IDisposable
    {
        protected TTransport transport = null;
        protected MemoryStream writeBuffer;
        protected MemoryStream readBuffer = null;

        private const int header_size = 4;
        private byte[] header_dummy = new byte[header_size]; 

        public class Factory : TTransportFactory
        {
            public override TTransport GetTransport(TTransport trans)
            {
                return new TZlibTransport(trans);
            }
        }

        public TZlibTransport()
        {
            writeBuffer = new MemoryStream(1024);
            readBuffer = new MemoryStream(1024);
        }

        public TZlibTransport(TTransport transport)
            : this()
        {
            this.transport = transport;
        }

        public override void Open()
        {
            transport.Open();
        }

        public override bool IsOpen
        {
            get
            {
                return transport.IsOpen;
            }
        }

        public override void Close()
        {
            transport.Close();
        }

        public override int Read(byte[] buf, int off, int len)
        {
            if (readBuffer != null)
            {
                int got = readBuffer.Read(buf, off, len);
                if (got > 0)
                {
                    return got;
                }
            }

            // Read another frame of data
            ReadFrame();

            return readBuffer.Read(buf, off, len);
        }

        private void ReadFrame()
        {
            byte[] i32rd = new byte[header_size];
            transport.ReadAll(i32rd, 0, header_size);
            int size =
                ((i32rd[0] & 0xff) << 24) |
                ((i32rd[1] & 0xff) << 16) |
                ((i32rd[2] & 0xff) << 8) |
                ((i32rd[3] & 0xff));


            byte[] buff = new byte[size];
            transport.ReadAll(buff, 0, size);
            var deCompressBuff = ZLibNet.ZLibCompressor.DeCompress(buff);

            //重置流，以复用该流的缓冲区准备下次的写入
            readBuffer.Seek(0, SeekOrigin.Begin);
            readBuffer.Write(deCompressBuff, 0, deCompressBuff.Length);
            readBuffer.Seek(0, SeekOrigin.Begin);
        }

        public override void Write(byte[] buf, int off, int len)
        {
            writeBuffer.Write(buf, off, len);
        }

        public override void Flush()
        {
            if (writeBuffer.Length < 0)
                throw new System.InvalidOperationException(); // logic error actually

            byte[] buf = new byte[writeBuffer.Length];
            writeBuffer.Seek(0, SeekOrigin.Begin);
            writeBuffer.Read(buf, 0, buf.Length);

            //重置流，以复用该流的缓冲区准备下次的写入
            writeBuffer.SetLength(0);

            //压缩字节流
            var compressBuffer = ZLibNet.ZLibCompressor.Compress(buf);
            int data_len = compressBuffer.Length;

            // 写入长度
            header_dummy[0] = (byte)(0xff & (data_len >> 24));
            header_dummy[1] = (byte)(0xff & (data_len >> 16));
            header_dummy[2] = (byte)(0xff & (data_len >> 8));
            header_dummy[3] = (byte)(0xff & (data_len));

            transport.Write(header_dummy);

            // 写入压缩后的流
            transport.Write(compressBuffer);

            transport.Flush();
        }

        #region " IDisposable Support "
        private bool _IsDisposed;

        // IDisposable
        protected override void Dispose(bool disposing)
        {
            if (!_IsDisposed)
            {
                if (disposing)
                {
                    if (readBuffer != null)
                        readBuffer.Dispose();
                    if (writeBuffer != null)
                        writeBuffer.Dispose();
                }
            }
            _IsDisposed = true;
        }
        #endregion
    }
}
