using System;

namespace com.baidu.dsf
{

	/// 
	/// <summary>
	/// @author arksea
	/// </summary>
	public class DSFrameworkException : Exception
	{

		public DSFrameworkException(string msg) : base(msg)
		{
		}

		public DSFrameworkException(string msg, Exception ex) : base(msg, ex)
		{
		}
	}

}