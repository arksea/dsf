using System;

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
    /// Provides the implementation for the common attributes shared by the
    /// sub-classes. New instances of this class will be created using the defaults
    /// defined by the public constants.
    /// <para>
    /// This class is not thread-safe.
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0
    /// </para>
    /// </summary>
    public abstract class BaseObjectPoolConfig : ICloneable
    {

        /// <summary>
        /// The default value for the {@code lifo} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getLifo() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getLifo() </seealso>
        public const bool DEFAULT_LIFO = true;

        /// <summary>
        /// The default value for the {@code maxWait} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getMaxWaitMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getMaxWaitMillis() </seealso>
        public const long DEFAULT_MAX_WAIT_MILLIS = -1L;

        /// <summary>
        /// The default value for the {@code minEvictableIdleTimeMillis}
        /// configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getMinEvictableIdleTimeMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getMinEvictableIdleTimeMillis() </seealso>
        public static readonly long DEFAULT_MIN_EVICTABLE_IDLE_TIME_MILLIS = 1000L * 60L * 30L;

        /// <summary>
        /// The default value for the {@code softMinEvictableIdleTimeMillis}
        /// configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getSoftMinEvictableIdleTimeMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getSoftMinEvictableIdleTimeMillis() </seealso>
        public const long DEFAULT_SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS = -1;

        /// <summary>
        /// The default value for the {@code numTestsPerEvictionRun} configuration
        /// attribute. </summary>
        /// <seealso cref= GenericObjectPool#getNumTestsPerEvictionRun() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getNumTestsPerEvictionRun() </seealso>
        public const int DEFAULT_NUM_TESTS_PER_EVICTION_RUN = 3;

        /// <summary>
        /// The default value for the {@code testOnBorrow} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getTestOnBorrow() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTestOnBorrow() </seealso>
        public const bool DEFAULT_TEST_ON_BORROW = false;

        /// <summary>
        /// The default value for the {@code testOnReturn} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getTestOnReturn() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTestOnReturn() </seealso>
        public const bool DEFAULT_TEST_ON_RETURN = false;

        /// <summary>
        /// The default value for the {@code testWhileIdle} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getTestWhileIdle() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTestWhileIdle() </seealso>
        public const bool DEFAULT_TEST_WHILE_IDLE = false;

        /// <summary>
        /// The default value for the {@code timeBetweenEvictionRunsMillis}
        /// configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getTimeBetweenEvictionRunsMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTimeBetweenEvictionRunsMillis() </seealso>
        public const long DEFAULT_TIME_BETWEEN_EVICTION_RUNS_MILLIS = -1L;

        /// <summary>
        /// The default value for the {@code blockWhenExhausted} configuration
        /// attribute. </summary>
        /// <seealso cref= GenericObjectPool#getBlockWhenExhausted() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getBlockWhenExhausted() </seealso>
        public const bool DEFAULT_BLOCK_WHEN_EXHAUSTED = true;

        /// <summary>
        /// The default value for enabling JMX for pools created with a configuration
        /// instance.
        /// </summary>
        public const bool DEFAULT_JMX_ENABLE = true;

        /// <summary>
        /// The default value for the prefix used to name JMX enabled pools created
        /// with a configuration instance. </summary>
        /// <seealso cref= GenericObjectPool#getJmxName() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getJmxName() </seealso>
        public const string DEFAULT_JMX_NAME_PREFIX = "pool";

        /// <summary>
        /// The default value for the base name to use to name JMX enabled pools
        /// created with a configuration instance. The default is <code>null</code>
        /// which means the pool will provide the base name to use. </summary>
        /// <seealso cref= GenericObjectPool#getJmxName() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getJmxName() </seealso>
        public const string DEFAULT_JMX_NAME_BASE = null;

        /// <summary>
        /// The default value for the {@code evictionPolicyClassName} configuration
        /// attribute. </summary>
        /// <seealso cref= GenericObjectPool#getEvictionPolicyClassName() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getEvictionPolicyClassName() </seealso>
        public const string DEFAULT_EVICTION_POLICY_CLASS_NAME = "org.apache.commons.pool2.impl.DefaultEvictionPolicy";


        private bool lifo = DEFAULT_LIFO;

        private long maxWaitMillis = DEFAULT_MAX_WAIT_MILLIS;

        private long minEvictableIdleTimeMillis = DEFAULT_MIN_EVICTABLE_IDLE_TIME_MILLIS;

        private long softMinEvictableIdleTimeMillis = DEFAULT_MIN_EVICTABLE_IDLE_TIME_MILLIS;

        private int numTestsPerEvictionRun = DEFAULT_NUM_TESTS_PER_EVICTION_RUN;

        private string evictionPolicyClassName = DEFAULT_EVICTION_POLICY_CLASS_NAME;

        private bool testOnBorrow = DEFAULT_TEST_ON_BORROW;

        private bool testOnReturn = DEFAULT_TEST_ON_RETURN;

        private bool testWhileIdle = DEFAULT_TEST_WHILE_IDLE;

        private long timeBetweenEvictionRunsMillis = DEFAULT_TIME_BETWEEN_EVICTION_RUNS_MILLIS;

        private bool blockWhenExhausted = DEFAULT_BLOCK_WHEN_EXHAUSTED;

        private bool jmxEnabled = DEFAULT_JMX_ENABLE;

        // TODO Consider changing this to a single property for 3.x
        private string jmxNamePrefix = DEFAULT_JMX_NAME_PREFIX;

        private string jmxNameBase = DEFAULT_JMX_NAME_PREFIX;


        /// <summary>
        /// Get the value for the {@code lifo} configuration attribute for pools
        /// created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code lifo} for this configuration
        ///          instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getLifo() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getLifo() </seealso>
        public virtual bool Lifo
        {
            get
            {
                return lifo;
            }
            set
            {
                this.lifo = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code maxWait} configuration attribute for pools
        /// created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code maxWait} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getMaxWaitMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getMaxWaitMillis() </seealso>
        public virtual long MaxWaitMillis
        {
            get
            {
                return maxWaitMillis;
            }
            set
            {
                this.maxWaitMillis = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code minEvictableIdleTimeMillis} configuration
        /// attribute for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code minEvictableIdleTimeMillis} for
        ///          this configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getMinEvictableIdleTimeMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getMinEvictableIdleTimeMillis() </seealso>
        public virtual long MinEvictableIdleTimeMillis
        {
            get
            {
                return minEvictableIdleTimeMillis;
            }
            set
            {
                this.minEvictableIdleTimeMillis = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code softMinEvictableIdleTimeMillis}
        /// configuration attribute for pools created with this configuration
        /// instance.
        /// </summary>
        /// <returns>  The current setting of {@code softMinEvictableIdleTimeMillis}
        ///          for this configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getSoftMinEvictableIdleTimeMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getSoftMinEvictableIdleTimeMillis() </seealso>
        public virtual long SoftMinEvictableIdleTimeMillis
        {
            get
            {
                return softMinEvictableIdleTimeMillis;
            }
            set
            {
                this.softMinEvictableIdleTimeMillis = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code numTestsPerEvictionRun} configuration
        /// attribute for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code numTestsPerEvictionRun} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getNumTestsPerEvictionRun() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getNumTestsPerEvictionRun() </seealso>
        public virtual int NumTestsPerEvictionRun
        {
            get
            {
                return numTestsPerEvictionRun;
            }
            set
            {
                this.numTestsPerEvictionRun = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code testOnBorrow} configuration attribute for
        /// pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code testOnBorrow} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getTestOnBorrow() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTestOnBorrow() </seealso>
        public virtual bool TestOnBorrow
        {
            get
            {
                return testOnBorrow;
            }
            set
            {
                this.testOnBorrow = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code testOnReturn} configuration attribute for
        /// pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code testOnReturn} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getTestOnReturn() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTestOnReturn() </seealso>
        public virtual bool TestOnReturn
        {
            get
            {
                return testOnReturn;
            }
            set
            {
                this.testOnReturn = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code testWhileIdle} configuration attribute for
        /// pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code testWhileIdle} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getTestWhileIdle() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTestWhileIdle() </seealso>
        public virtual bool TestWhileIdle
        {
            get
            {
                return testWhileIdle;
            }
            set
            {
                this.testWhileIdle = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code timeBetweenEvictionRunsMillis} configuration
        /// attribute for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code timeBetweenEvictionRunsMillis} for
        ///          this configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getTimeBetweenEvictionRunsMillis() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getTimeBetweenEvictionRunsMillis() </seealso>
        public virtual long TimeBetweenEvictionRunsMillis
        {
            get
            {
                return timeBetweenEvictionRunsMillis;
            }
            set
            {
                this.timeBetweenEvictionRunsMillis = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code evictionPolicyClassName} configuration
        /// attribute for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code evictionPolicyClassName} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getEvictionPolicyClassName() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getEvictionPolicyClassName() </seealso>
        public virtual string EvictionPolicyClassName
        {
            get
            {
                return evictionPolicyClassName;
            }
            set
            {
                this.evictionPolicyClassName = value;
            }
        }


        /// <summary>
        /// Get the value for the {@code blockWhenExhausted} configuration attribute
        /// for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code blockWhenExhausted} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getBlockWhenExhausted() </seealso>
        /// <seealso cref= GenericKeyedObjectPool#getBlockWhenExhausted() </seealso>
        public virtual bool BlockWhenExhausted
        {
            get
            {
                return blockWhenExhausted;
            }
            set
            {
                this.blockWhenExhausted = value;
            }
        }


        /// <summary>
        /// Gets the value of the flag that determines if JMX will be enabled for
        /// pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code jmxEnabled} for this configuration
        ///          instance </returns>
        public virtual bool JmxEnabled
        {
            get
            {
                return jmxEnabled;
            }
            set
            {
                this.jmxEnabled = value;
            }
        }


        /// <summary>
        /// Gets the value of the JMX name base that will be used as part of the
        /// name assigned to JMX enabled pools created with this configuration
        /// instance. A value of <code>null</code> means that the pool will define
        /// the JMX name base.
        /// </summary>
        /// <returns>  The current setting of {@code jmxNameBase} for this
        ///          configuration instance </returns>
        public virtual string JmxNameBase
        {
            get
            {
                return jmxNameBase;
            }
            set
            {
                this.jmxNameBase = value;
            }
        }


        /// <summary>
        /// Gets the value of the JMX name prefix that will be used as part of the
        /// name assigned to JMX enabled pools created with this configuration
        /// instance.
        /// </summary>
        /// <returns>  The current setting of {@code jmxNamePrefix} for this
        ///          configuration instance </returns>
        public virtual string JmxNamePrefix
        {
            get
            {
                return jmxNamePrefix;
            }
            set
            {
                this.jmxNamePrefix = value;
            }
        }

        public virtual object Clone()
        {
            return base.MemberwiseClone();
        }

    }

}