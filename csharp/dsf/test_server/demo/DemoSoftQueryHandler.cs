using System;
using System.Threading;
using System.Collections;
using System.Collections.Generic;

namespace com.baidu.dsf.demo.server
{

    using baidu91.appsvc.demo;
    using TException = Thrift.TException;
    using java.util.concurrent.atomic;
    using com.baidu.dsf.core;


    /// 
    /// <summary>
    /// @author xhx
    /// </summary>
    public class DemoSoftQueryHandler : DemoSoftQuery.Iface
    {
        private readonly AtomicLong requestDelay = new AtomicLong(0L); //模拟request时间

        public int test1()
        {
            return 10;
        }

        public Dictionary<string, string> test2()
        {
            Dictionary<string, string> dic = new Dictionary<string, string>();
            dic.Add("key1", "value1");
            dic.Add("key2", "value2");
            return dic;
        }
        public void ping()
        {
            delayOnce();
        }

        internal virtual void delayRequestOnce(long rt)
        {
            requestDelay.set(rt);
        }

        private void delayOnce()
        {
            long delay = requestDelay.getAndSet(0L);
            if (delay > 0)
            {
                try
                {
                    DSFLogHelper.InfoLog("DemoSoftQueryHandler","delayOnce");
                    java.lang.Thread.sleep(delay);
                }
                catch (Exception)
                {
                }
            }
        }

        public List<SimpleSoft> getAll()
        {
            delayOnce();
            List<SimpleSoft> list = new List<SimpleSoft>();
            SimpleSoft s = new SimpleSoft();
            s.softid = 1234567;
            s.name = "hello world!";
            s.vender = "baidu91";
            list.Add(s);
            return list;
        }

        public SimpleSoft getByID(long id)
        {
            //logger.info("getByID("+id+")");
            delayOnce();
            SimpleSoft s = new SimpleSoft();
            s.softid = id;
            s.name = "hello world!";
            s.vender = "baidu91";
            return s;
        }

        public List<SimpleSoft> getByVender(string vender)
        {
            delayOnce();
            DSFLogHelper.InfoLog("DemoSoftQueryHandler", "getByVender(" + vender + ")");
            List<SimpleSoft> list = new List<SimpleSoft>();
            SimpleSoft s = new SimpleSoft();
            s.softid = 1234567;
            s.name = "hello world!";
            s.vender = vender;
            list.Add(s);
            return list;
        }

        public void put(long id, SimpleSoft soft)
        {
            delayOnce();
            DSFLogHelper.InfoLog("DemoSoftQueryHandler", "put(" + id + "," + soft + ")");
        }

    }

}