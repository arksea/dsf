using com.baidu.dsf.core;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace com.baidu.dsf.register
{
    /// <summary>
    /// 与注册服务器的json通信协议编码
    /// </summary>
    public class JsonCodec
    {
        private Dictionary<string, Type> _typeMap = new Dictionary<string, Type>();
        private Dictionary<Type, string> _classMap = new Dictionary<Type, string>();

        public JsonCodec()
        {
            InitTypeMap();
        }

        private void InitTypeMap()
        {
            AddTypeMap("string", typeof(string));
            AddTypeMap("notify_svc_state", typeof(MsgNotifySvcState));
            AddTypeMap("subscribe_result", typeof(MsgSubscribeResult));
            AddTypeMap("query_svcdef_result", typeof(MsgServiceDefine));
            AddTypeMap("notify_svcdef_update", typeof(MsgServiceDefineUpdate));
        }

        private void AddTypeMap(string name, Type type)
        {
            _typeMap.Add(name, type);
            _classMap.Add(type, name);
        }

        /// <summary>
        /// 解码消息内容
        /// </summary>
        /// <param name="msg">消息内容字符串</param>
        /// <returns></returns>
        public Message decodec(String msg)
        {
            String[] pair = msg.Split(new char[] { '=' }, 2); 
            Object obj;
            //subscribe_result的层数太深，Newtonsoft的bug造成无法将其中的states反序列化，所以手工构造对象
            if (pair[0].Equals("subscribe_result"))
            {
                JObject jobj = (JObject)JsonConvert.DeserializeObject(pair[1]);
                //构造svcdef
                MsgServiceDefine svcdef = jobj["svcdef"].ToObject<MsgServiceDefine>();
                //构造svclist
                ICollection<ServiceInstance> svclist = new List<ServiceInstance>();
                foreach (var svc in jobj["svclist"])
                {
                    MsgSvcAddr addr = svc["addr"].ToObject<MsgSvcAddr>();
                    ServiceInstance inst = new ServiceInstance(addr);
                    foreach (JProperty st in svc["states"])
                    {
                        inst.states.put(st.Name, st.Value.ToString());
                    }
                    svclist.Add(inst);
                }
                obj = new MsgSubscribeResult(svcdef, svclist);
            }
            else
            {
                Type type;
                if (!_typeMap.TryGetValue(pair[0], out type))
                {
                    type = typeof(String);
                }
                obj = JsonConvert.DeserializeObject(pair[1], type);
            }
            Message d = new Message(pair[0], obj);
            return d;
        }
        /// <summary>
        /// 编码消息内容
        /// </summary>
        /// <param name="name">名称</param>
        /// <param name="obj">对象</param>
        /// <returns></returns>
        public string encodec(string name, object obj)
        {
            string value = JsonConvert.SerializeObject(obj, Formatting.None);
            return name + "=" + value;
        }
        /// <summary>
        /// 编码消息内容
        /// </summary>
        /// <param name="msg">消息内容实体</param>
        /// <returns></returns>
        public string encodec(Message msg)
        {
            return encodec(msg.name, msg.value);
        }
    }
}
