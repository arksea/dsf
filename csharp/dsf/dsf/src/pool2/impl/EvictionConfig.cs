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
    /// This class is used by pool implementations to pass configuration information
    /// to <seealso cref="EvictionPolicy"/> instances. The <seealso cref="EvictionPolicy"/> may also have
    /// its own specific configuration attributes.
    /// <para>
    /// This class is immutable and thread-safe.
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0
    /// </para>
    /// </summary>
    public class EvictionConfig
    {

        private readonly long idleEvictTime;
        private readonly long idleSoftEvictTime;
        private readonly int minIdle;


        /// <summary>
        /// Create a new eviction configuration with the specified parameters.
        /// Instances are immutable.
        /// </summary>
        /// <param name="poolIdleEvictTime"> Expected to be provided by
        ///        <seealso cref="BaseGenericObjectPool#getMinEvictableIdleTimeMillis()"/> </param>
        /// <param name="poolIdleSoftEvictTime"> Expected to be provided by
        ///        <seealso cref="BaseGenericObjectPool#getSoftMinEvictableIdleTimeMillis()"/> </param>
        /// <param name="minIdle"> Expected to be provided by
        ///        <seealso cref="GenericObjectPool#getMinIdle()"/> or
        ///        <seealso cref="GenericKeyedObjectPool#getMinIdlePerKey()"/> </param>
        public EvictionConfig(long poolIdleEvictTime, long poolIdleSoftEvictTime, int minIdle)
        {
            if (poolIdleEvictTime > 0)
            {
                idleEvictTime = poolIdleEvictTime;
            }
            else
            {
                idleEvictTime = long.MaxValue;
            }
            if (poolIdleSoftEvictTime > 0)
            {
                idleSoftEvictTime = poolIdleSoftEvictTime;
            }
            else
            {
                idleSoftEvictTime = long.MaxValue;
            }
            this.minIdle = minIdle;
        }

        /// <summary>
        /// Obtain the {@code idleEvictTime} for this eviction configuration
        /// instance.
        /// <para>
        /// How the evictor behaves based on this value will be determined by the
        /// configured <seealso cref="EvictionPolicy"/>.
        /// 
        /// </para>
        /// </summary>
        /// <returns> The {@code idleEvictTime} in milliseconds </returns>
        public virtual long IdleEvictTime
        {
            get
            {
                return idleEvictTime;
            }
        }

        /// <summary>
        /// Obtain the {@code idleSoftEvictTime} for this eviction configuration
        /// instance.
        /// <para>
        /// How the evictor behaves based on this value will be determined by the
        /// configured <seealso cref="EvictionPolicy"/>.
        /// 
        /// </para>
        /// </summary>
        /// <returns> The (@code idleSoftEvictTime} in milliseconds </returns>
        public virtual long IdleSoftEvictTime
        {
            get
            {
                return idleSoftEvictTime;
            }
        }

        /// <summary>
        /// Obtain the {@code minIdle} for this eviction configuration instance.
        /// <para>
        /// How the evictor behaves based on this value will be determined by the
        /// configured <seealso cref="EvictionPolicy"/>.
        /// 
        /// </para>
        /// </summary>
        /// <returns> The {@code minIdle} </returns>
        public virtual int MinIdle
        {
            get
            {
                return minIdle;
            }
        }
    }

}