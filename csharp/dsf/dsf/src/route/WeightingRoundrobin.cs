using System;
using System.Collections.Generic;

namespace com.baidu.dsf.route
{

	using Service = com.baidu.dsf.register.Service;
	using ServiceInstance = com.baidu.dsf.register.ServiceInstance;

	/// <summary>
	/// 带权重的轮询路由策略，所有在线服务实例可实时更新自己的重复权重参数（重复调用次数）route-weight
	/// 没有指定则默认为1
	/// @author arksea
	/// </summary>
	public class WeightingRoundrobin : AbstractRouteStrategy
	{

		private int index = 0; //当前引用的服务实例索引
		private int count = 0; //当前实例已调用的次数

		public WeightingRoundrobin(Service s) : base(s)
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
                    ServiceInstance s = (ServiceInstance)list.get(index);
					string onlineStr = (string) s.getState("online");
					bool online = (onlineStr != null && onlineStr.Equals("true"));
					if (online)
					{
						++onlineCount;
					}
					string weightStr = (string) s.getState("route-weight");
					int weight = 1;
					if (weightStr != null)
					{
						try
						{
							weight = Convert.ToInt32(weightStr);
						}
						catch (Exception)
						{
						}
					}
					if (online && ++count <= weight && s.tryAcquire())
					{
						return s;
					}
					else
					{
						count = 0;
						++index;
					}
				}
				if (onlineCount == 0)
				{
					throw new NoUseableServiceException(service.define.name);
				}
				throw new RateLimitedException(service.define.name);
			}
		}

		public virtual void next()
		{
			//换下一个实例
			count = 0;
			++index;
		}
	}

}