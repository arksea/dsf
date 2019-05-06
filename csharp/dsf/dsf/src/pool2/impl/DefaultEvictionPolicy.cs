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

    using org.apache.commons.pool2;

    /// <summary>
    /// Provides the default implementation of <seealso cref="EvictionPolicy"/> used by the
    /// pools. Objects will be evicted if the following conditions are met:
    /// <ul>
    /// <li>the object has been idle longer than
    ///     <seealso cref="GenericObjectPool#getMinEvictableIdleTimeMillis()"/> /
    ///     <seealso cref="GenericKeyedObjectPool#getMinEvictableIdleTimeMillis()"/></li>
    /// <li>there are more than <seealso cref="GenericObjectPool#getMinIdle()"/> /
    ///     <seealso cref="GenericKeyedObjectPoolConfig#getMinIdlePerKey()"/> idle objects in
    ///     the pool and the object has been idle for longer than
    ///     <seealso cref="GenericObjectPool#getSoftMinEvictableIdleTimeMillis()"/> /
    ///     <seealso cref="GenericKeyedObjectPool#getSoftMinEvictableIdleTimeMillis()"/>
    /// </ul>
    /// This class is immutable and thread-safe.
    /// </summary>
    /// @param <T> the type of objects in the pool
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0 </param>
    public class DefaultEvictionPolicy<T> : EvictionPolicy<T>
    {

        public bool evict(EvictionConfig config, PooledObject<T> underTest, int idleCount)
        {

            if ((config.IdleSoftEvictTime < underTest.IdleTimeMillis && config.MinIdle < idleCount) || config.IdleEvictTime < underTest.IdleTimeMillis)
            {
                return true;
            }
            return false;
        }
    }

}