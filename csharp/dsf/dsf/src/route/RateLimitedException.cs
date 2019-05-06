using System;

namespace com.baidu.dsf.route
{

	/// 
	/// <summary>
	/// @author arksea
	/// </summary>
	public class RateLimitedException : Exception
	{

		public RateLimitedException(string msg) : base(msg)
		{
		}

		public RateLimitedException(string msg, Exception ex) : base(msg, ex)
		{
		}
	}

}