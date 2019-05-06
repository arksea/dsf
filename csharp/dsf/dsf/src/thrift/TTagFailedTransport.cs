using System;

namespace com.baidu.dsf.thrift
{

	using Thrift.Transport;

	/// <summary>
	/// @author arksea
	/// 用于标记失败连接，catch所有TransportException,打上标记后重新抛出，
	/// 连接池将使用这个标记丢弃失败的连接。
	/// 此类非线程安全，所以获取的Client必须由同一个线程调用及归还到连接池
	/// </summary>
	public class TTagFailedTransport : TTransport
	{

		private readonly TTransport transport;
		private bool __failed = false; //双下划线是为了提醒必须通过getter、setter使用
		private Exception __failedException = null;

		public TTagFailedTransport(TTransport transport)
		{
			this.transport = transport;
		}

		public virtual bool Failed
		{
			get
			{
				lock (this)
				{
					return __failed;
				}
			}
		}

		public virtual Exception FailedException
		{
			get
			{
				lock (this)
				{
					return __failedException;
				}
			}
		}

		private void failed(Exception ex)
		{
			lock (this)
			{
				__failed = true;
				__failedException = ex;
			}
		}

		public override bool IsOpen
		{
            get { return transport.IsOpen; }
        }

		public override void Open()
		{
			try
			{
				transport.Open();
			}
			catch (TTransportException ex)
			{
				failed(ex);
				throw ex;
			}
		}

		public override void Close()
		{
			transport.Close();
		}

		public override int Read(byte[] buf, int off, int len)
		{
			try
			{
				return transport.Read(buf, off, len);
			}
			catch (TTransportException ex)
			{
				failed(ex);
				throw ex;
			}
		}

		public override int ReadAll(byte[] buf, int off, int len)
		{
			try
			{
				return transport.ReadAll(buf, off, len);
			}
			catch (TTransportException ex)
			{
				failed(ex);
				throw ex;
			}
		}

		public override void Write(byte[] buf)
		{
			try
			{
				transport.Write(buf);
			}
			catch (TTransportException ex)
			{
				failed(ex);
				throw ex;
			}
		}

		public override void Write(byte[] buf, int off, int len)
		{
			try
			{
				transport.Write(buf, off, len);
			}
			catch (TTransportException ex)
			{
				failed(ex);
				throw ex;
			}
		}

		public override void Flush()
		{
			try
			{
				transport.Flush();
			}
			catch (TTransportException ex)
			{
				failed(ex);
				throw ex;
			}
		}

        public override IAsyncResult BeginFlush(AsyncCallback callback, object state)
        {
            try
            {
                return transport.BeginFlush(callback, state);
            }
            catch (TTransportException ex)
            {
                failed(ex);
                throw;
            }
        }

        public override void EndFlush(IAsyncResult asyncResult)
        {
            try
            {
                transport.EndFlush(asyncResult);
            }
            catch (Exception ex)
            {
                failed(ex);
                throw;
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                transport.Dispose();
            }
        }
	}

}