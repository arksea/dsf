package dsf.pool;

/**
 * 失败处理策略
 * @author arksea
 */
public enum FailStrategy {

    FAILOVER, //失败重试
    FAILFAST, //失败报错
    FAILSAFE    //失败安全，不作任何处理直接返回（null）
}
