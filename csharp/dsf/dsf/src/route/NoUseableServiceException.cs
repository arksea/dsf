using System;

namespace com.baidu.dsf.route
{

	/// 
	/// <summary>
	/// @author xiaohaixing
	/// </summary>
	public class NoUseableServiceException : Exception
	{

		public NoUseableServiceException(string msg) : base(msg)
		{
		}

		public NoUseableServiceException(string msg, Exception ex) : base(msg, ex)
		{
		}
	}

}