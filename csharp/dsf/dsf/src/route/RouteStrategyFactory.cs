namespace com.baidu.dsf.route
{

	using Service = com.baidu.dsf.register.Service;

	/// <summary>
	/// 路由策略工厂类，根据类型串选择路由策略，默认为带权重的轮询策略
	/// @author arksea
	/// </summary>
	public class RouteStrategyFactory
	{

		public static IRouteStrategy create(string type, Service service)
		{
			if (type.Equals("roundrobin"))
			{
				return new Roundrobin(service);
			}
			else if (type.Equals("weighting-roundrobin"))
			{
				return new WeightingRoundrobin(service);
			}
			else if (type.Equals("hot-standby"))
			{
				return new HotStandby(service);
			}
			else
			{
				return new WeightingRoundrobin(service);
			}
		}
	}

}