﻿using System.Collections.Generic;

namespace com.baidu.dsf.register
{

    /// <summary>
    /// 服务定义消息
    /// {
    ///  "regname":"service1 ver1.0",   //服务注册名,通常包含路径、服务名与版本信息
    ///  "name": "com.xxx.Class1",      //服务名，通常使用类名，以便于框架定位并创建其实例
    ///  "version":"1.0",               //服务版本号
    ///  "props":
    ///   {
    ///      "protocol":"Thrift.binary"     //协议类型
    ///      "route_strategy":"roundrobin"   //路由策略
    ///      "timeout":"3000",
    ///      "fail_strategy":"xxx"           // 失败转移策略
    ///   }
    /// }
    /// @author arksea
    /// </summary>
    public class MsgServiceDefine
    {

        public readonly string regname;
        public readonly string name;
        public readonly string version;
        internal string description;
        internal IDictionary<string, string> props;

        public MsgServiceDefine(string regname, string name, string version, string description, IDictionary<string, string> props)
        {
            this.regname = regname;
            this.name = name;
            this.version = version;
            this.props = props;
            this.description = description;
        }

        public virtual void update(MsgServiceDefine o)
        {
            //regname、name与version不允许修改
            description = o.description;
            props.Clear();
            foreach (KeyValuePair<string, string> e in props)
            {
                props.Add(e.Key, e.Value);
            }
        }

		public virtual string getProperty(string name)
		{
            if (props.ContainsKey(name))
            {
                string value = props[name];
                return value;
            }
            else
            {
                return null;
            }
		}


        public virtual string Description
        {
            get
            {
                return description;
            }
        }

    }

}