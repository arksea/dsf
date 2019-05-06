using System.Collections.Generic;

namespace com.baidu.dsf.route
{

	using Service = com.baidu.dsf.register.Service;
	using ServiceInstance = com.baidu.dsf.register.ServiceInstance;

	/// <summary>
	/// 轮询的路由策略，所有在线服务实例拥有相同的权重
	/// @author arksea
	/// </summary>
	public class Roundrobin : AbstractRouteStrategy
	{

		private int index = 0;

		public Roundrobin(Service s) : base(s)
		{
		}

		public override ServiceInstance Instance
		{
			get
			{
				java.util.List list = service.instanceList;
				int onlineCount = 0;
				for (int i = 0; i < list.size(); ++i)
				{
					if (index >= list.size())
					{
						index = 0;
					}
                    ServiceInstance s = (ServiceInstance)list.get(i);
					++index;
					string onlineStr = (string) s.getState("online");
					bool online = (onlineStr != null && onlineStr.Equals("true"));
					if (online)
					{
						++onlineCount;
						if (s.tryAcquire())
						{
							return s;
						}
					}
				}
				if (onlineCount == 0)
				{
					throw new NoUseableServiceException(service.define.name);
				}
				throw new RateLimitedException(service.define.name);
			}
		}
	}

}