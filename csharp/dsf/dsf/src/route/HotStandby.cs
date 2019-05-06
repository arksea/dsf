using System.Collections.Generic;

namespace com.baidu.dsf.route
{

	using Service = com.baidu.dsf.register.Service;
	using ServiceInstance = com.baidu.dsf.register.ServiceInstance;

	/// <summary>
	/// 热备的路由策略，按服务器地址排序，选择第一个在线的服务
	/// @author arksea
	/// </summary>
	public class HotStandby : AbstractRouteStrategy
	{

		public HotStandby(Service s) : base(s)
		{
		}

		public override ServiceInstance Instance
		{
			get
			{
				java.util.List list = service.instanceList;
                SortedSet<ServiceInstance> sorted = new SortedSet<ServiceInstance>();
                java.util.Iterator it = list.iterator();
                while(it.hasNext())
                {
                    ServiceInstance s = (ServiceInstance)it.next();
                    sorted.Add(s);
                }
				int onlineCount = 0;
				foreach (ServiceInstance s in sorted)
				{
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