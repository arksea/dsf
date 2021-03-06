﻿namespace com.baidu.dsf.pool
{

    /// <summary>
    /// 失败处理策略
    /// @author arksea
    /// </summary>
    public enum FailStrategy
    {

        FAILOVER, //失败重试
        FAILFAST, //失败报错
        FAILSAFE //失败安全，不作任何处理直接返回（null）
    }

}