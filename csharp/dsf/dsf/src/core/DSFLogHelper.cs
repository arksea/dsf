using net91com.Core.Util;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace com.baidu.dsf.core
{
    public static class DSFLogHelper
    {
        public static bool EnableInfoLog;
        public static bool EnableDebugLog;
        public static bool EnableConsoleLog;
        public static object _lock = new Object();

        static DSFLogHelper()
        {
            DSFLogHelper.EnableInfoLog = ConfigHelper.GetBoolean("DSF_EnableInfoLog");
            DSFLogHelper.EnableDebugLog = ConfigHelper.GetBoolean("DSF_EnableDebugLog");
            DSFLogHelper.EnableConsoleLog = ConfigHelper.GetBoolean("DSF_EnableConsoleLog");
        }

        public static Action<string> WriteToConsole = new Action<string>(Console.WriteLine);
        public static void InfoLog(string source, string message)
        {
            if (EnableInfoLog)
            {
                net91com.Core.Util.LogHelper.WriteCustom(message, "DSF\\Info\\" + source, false);
            }
            ConsoleLog(string.Format("#{2} {0}\r\n{1}", source, message, DateTime.Now));
        }

        public static void DebugLog(string source, string message)
        {
            if (EnableDebugLog)
            {
                net91com.Core.Util.LogHelper.WriteCustom(message, "DSF\\Debug\\" + source, false);
            }
            ConsoleLog(string.Format("#{2} {0}\r\n{1}", source, message, DateTime.Now));
        }

        public static void ErrorLog(string source, Exception ex, string extMessage)
        {
            net91com.Core.Util.LogHelper.WriteException(source + ":" + extMessage, ex);
            ConsoleLog(string.Format("#{3} {0}\r\n{2}\r\n{1}", source, ex.ToString(), extMessage, DateTime.Now));
        }

        public static void ErrorLog(string source, string extMessage)
        {
            net91com.Core.Util.LogHelper.WriteError(source + ":" + extMessage);
            ConsoleLog(string.Format("#{2} {0}\r\n{1}", source, extMessage, DateTime.Now));
        }

        public static void WarnLog(string source, Exception ex, string extMessage)
        {
            net91com.Core.Util.LogHelper.WriteException(source + ":" + extMessage, ex);
            ConsoleLog(string.Format("#{3} {0}\r\n{2}\r\n{1}", source, ex.ToString(), extMessage, DateTime.Now));
        }

        public static void WarnLog(string source, string extMessage)
        {
            net91com.Core.Util.LogHelper.WriteError(source + ":" + extMessage);
            ConsoleLog(string.Format("#{2} {0}\r\n{1}", source, extMessage, DateTime.Now));
        }

        public static void ConsoleLog(string message)
        {
            //DEBUG模式默认都在控制台输出日志
#if DEBUG
            EnableConsoleLog = true;
#endif
            if (EnableConsoleLog)
            {
                lock (_lock)
                {
                    WriteToConsole(message);
                }
            }
        }

    }
}
