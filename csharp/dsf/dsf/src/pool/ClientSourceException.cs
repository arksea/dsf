using System;

namespace com.baidu.dsf.pool
{

    using MsgSvcAddr = com.baidu.dsf.register.MsgSvcAddr;

    /// 
    /// <summary>
    /// @author arksea
    /// </summary>
    public class ClientSourceException : Exception
    {

        private string regname;
        private MsgSvcAddr svcAddr;

        public ClientSourceException(string msg, string regname, MsgSvcAddr addr) : base(msg)
        {
            this.regname = regname;
            this.svcAddr = addr;
        }

        public ClientSourceException(string msg, string regname, MsgSvcAddr addr, Exception ex) : base(msg, ex)
        {
            this.regname = regname;
            this.svcAddr = addr;
        }

        public virtual string Regname
        {
            get
            {
                return regname;
            }
        }

        public virtual MsgSvcAddr SvcAddr
        {
            get
            {
                return svcAddr;
            }
        }
    }

}