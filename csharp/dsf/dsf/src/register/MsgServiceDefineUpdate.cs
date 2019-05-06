using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace com.baidu.dsf.register
{
    public class MsgServiceDefineUpdate : MsgServiceDefine
    {
        public MsgServiceDefineUpdate(string regname, string name, string version, string description, IDictionary<string, string> props) 
            : base(regname, name, version, description, props)
        {
        }
    }
}
