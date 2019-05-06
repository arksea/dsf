namespace com.baidu.dsf.thrift
{

	using Thrift.Protocol;
    using Thrift.Transport;


	/// 
	/// <summary>
	/// @author arksea
	/// </summary>
	public class ProtocolFactory
	{

		public static TProtocol create(string type, TTransport transport)
		{
			TProtocol protocol = null;
			if (type.Equals("binary"))
			{
				TTagFailedTransport tt = new TTagFailedTransport(transport);
				protocol = new TBinaryProtocol(tt);
			}
			else if (type.Equals("framed-binary"))
			{
				TTagFailedTransport tt = new TTagFailedTransport(new TFramedTransport(transport));
				protocol = new TBinaryProtocol(tt);
			}
			else if (type.Equals("zip-binary"))
			{
				TTagFailedTransport tt = new TTagFailedTransport(new TZlibTransport(transport));
				protocol = new TBinaryProtocol(tt);
			}
			else if (type.Equals("compact"))
			{
				TTagFailedTransport tt = new TTagFailedTransport(transport);
				protocol = new TCompactProtocol(tt);
			}
			else if (type.Equals("framed-compact"))
			{
				TTagFailedTransport tt = new TTagFailedTransport(new TFramedTransport(transport));
				protocol = new TCompactProtocol(tt);
			}
			else
			{
				TTagFailedTransport tt = new TTagFailedTransport(new TFramedTransport(transport));
				protocol = new TBinaryProtocol(tt);
			}
			return protocol;
		}
	}

}