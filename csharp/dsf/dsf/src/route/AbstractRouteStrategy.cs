namespace com.baidu.dsf.route
{

	using Service = com.baidu.dsf.register.Service;

	/// 
	/// <summary>
	/// @author arksea
	/// </summary>
	public abstract class AbstractRouteStrategy : IRouteStrategy
	{
		public abstract com.baidu.dsf.register.ServiceInstance Instance {get;}

		protected internal Service service;

		public AbstractRouteStrategy(Service svc)
		{
			this.service = svc;
		}
	}

}