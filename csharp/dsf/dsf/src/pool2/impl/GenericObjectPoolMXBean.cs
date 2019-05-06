/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
namespace org.apache.commons.pool2.impl
{

    /// <summary>
    /// Defines the methods that will be made available via JMX.
    /// 
    /// NOTE: This interface is subject to change between major version releases
    /// of commons pool.
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0
    /// </summary>
    public interface GenericObjectPoolMXBean
    {
        // Getters for basic configuration settings
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getBlockWhenExhausted()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getBlockWhenExhausted()"/> </returns>
        bool BlockWhenExhausted {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getLifo()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getLifo()"/> </returns>
        bool Lifo {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMaxIdle()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMaxIdle()"/> </returns>
        int MaxIdle {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMaxTotal()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMaxTotal()"/> </returns>
        int MaxTotal {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMaxWaitMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMaxWaitMillis()"/> </returns>
        long MaxWaitMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMinEvictableIdleTimeMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMinEvictableIdleTimeMillis()"/> </returns>
        long MinEvictableIdleTimeMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMinIdle()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMinIdle()"/> </returns>
        int MinIdle {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getNumActive()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getNumActive()"/> </returns>
        int NumActive {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getNumIdle()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getNumIdle()"/> </returns>
        int NumIdle {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getNumTestsPerEvictionRun()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getNumTestsPerEvictionRun()"/> </returns>
        int NumTestsPerEvictionRun {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getTestOnBorrow()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getTestOnBorrow()"/> </returns>
        bool TestOnBorrow {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getTestOnReturn()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getTestOnReturn()"/> </returns>
        bool TestOnReturn {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getTestWhileIdle()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getTestWhileIdle()"/> </returns>
        bool TestWhileIdle {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getTimeBetweenEvictionRunsMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getTimeBetweenEvictionRunsMillis()"/> </returns>
        long TimeBetweenEvictionRunsMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#isClosed()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#isClosed()"/> </returns>
        bool Closed {get;}
        // Getters for monitoring attributes
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getBorrowedCount()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getBorrowedCount()"/> </returns>
        long BorrowedCount {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getReturnedCount()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getReturnedCount()"/> </returns>
        long ReturnedCount {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getCreatedCount()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getCreatedCount()"/> </returns>
        long CreatedCount {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getDestroyedCount()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getDestroyedCount()"/> </returns>
        long DestroyedCount {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getDestroyedByEvictorCount()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getDestroyedByEvictorCount()"/> </returns>
        long DestroyedByEvictorCount {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getDestroyedByBorrowValidationCount()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getDestroyedByBorrowValidationCount()"/> </returns>
        long DestroyedByBorrowValidationCount {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMeanActiveTimeMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMeanActiveTimeMillis()"/> </returns>
        long MeanActiveTimeMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMeanIdleTimeMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMeanIdleTimeMillis()"/> </returns>
        long MeanIdleTimeMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMeanBorrowWaitTimeMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMeanBorrowWaitTimeMillis()"/> </returns>
        long MeanBorrowWaitTimeMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getMaxBorrowWaitTimeMillis()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getMaxBorrowWaitTimeMillis()"/> </returns>
        long MaxBorrowWaitTimeMillis {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getCreationStackTrace()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getCreationStackTrace()"/> </returns>
        string CreationStackTrace {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getNumWaiters()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getNumWaiters()"/> </returns>
        int NumWaiters {get;}

        // Getters for abandoned object removal configuration
        /// <summary>
        /// See <seealso cref="GenericObjectPool#isAbandonedConfig()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#isAbandonedConfig()"/> </returns>
        bool AbandonedConfig {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getLogAbandoned()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getLogAbandoned()"/> </returns>
        bool LogAbandoned {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getRemoveAbandonedOnBorrow()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getRemoveAbandonedOnBorrow()"/> </returns>
        bool RemoveAbandonedOnBorrow {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getRemoveAbandonedOnMaintenance()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getRemoveAbandonedOnMaintenance()"/> </returns>
        bool RemoveAbandonedOnMaintenance {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getRemoveAbandonedTimeout()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getRemoveAbandonedTimeout()"/> </returns>
        int RemoveAbandonedTimeout {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#getFactoryType()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#getFactoryType()"/> </returns>
        string FactoryType {get;}
        /// <summary>
        /// See <seealso cref="GenericObjectPool#listAllObjects()"/> </summary>
        /// <returns> See <seealso cref="GenericObjectPool#listAllObjects()"/> </returns>
        java.util.Set listAllObjects();
    }

}