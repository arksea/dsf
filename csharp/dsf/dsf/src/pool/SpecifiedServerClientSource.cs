using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace com.baidu.dsf.pool
{
    using com.baidu.dsf.adaptor;
    using com.baidu.dsf.register;
    class SpecifiedServerClientSource<I> : ClientSource<I>
    {
        private ServiceInstance instance;


        public SpecifiedServerClientSource(string regname, String host, int port, int timeout) : base(regname, timeout)
        {
            instance = new ServiceInstance(new MsgSvcAddr(host, port));
        }

        public override RawClientInfo getRowClient()
        {
            RawClientInfo raw = new RawClientInfo();
            raw.instance = this.instance;
            raw.client = creator.create(instance, timeout);
            return raw;
        }

        public override IList<RawClientInfo> getRowClients()
        {
            IList<RawClientInfo> infos = new List<RawClientInfo>();
            RawClientInfo raw = new RawClientInfo();
            raw.instance = this.instance;
            raw.client = creator.create(raw.instance, timeout);
            infos.Add(raw);
            return infos;
        }
    }

}
