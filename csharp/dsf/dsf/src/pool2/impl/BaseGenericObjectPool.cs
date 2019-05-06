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



    using org.apache.commons.pool2;
    using java.util.concurrent.atomic;
    using java.util;
    using java.lang.management;
    using javax.management;
    using java.io;

    /// <summary>
    /// Base class that provides common functionality for <seealso cref="GenericObjectPool"/>
    /// and <seealso cref="GenericKeyedObjectPool"/>. The primary reason this class exists is
    /// reduce code duplication between the two pool implementations.
    /// </summary>
    /// @param <T> Type of element pooled in this pool.
    /// 
    /// This class is intended to be thread-safe.
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0 </param>
    public abstract class BaseGenericObjectPool<T>
    {

        // Constants
        /// <summary>
        /// The size of the caches used to store historical data for some attributes
        /// so that rolling means may be calculated.
        /// </summary>
        public const int MEAN_TIMING_STATS_CACHE_SIZE = 100;

        // Configuration attributes
        private volatile int maxTotal = -1;
        private volatile bool blockWhenExhausted = BaseObjectPoolConfig.DEFAULT_BLOCK_WHEN_EXHAUSTED;
        private AtomicLong maxWaitMillis = new AtomicLong(BaseObjectPoolConfig.DEFAULT_MAX_WAIT_MILLIS);
        private volatile bool lifo = BaseObjectPoolConfig.DEFAULT_LIFO;
        private volatile bool testOnBorrow = BaseObjectPoolConfig.DEFAULT_TEST_ON_BORROW;
        private volatile bool testOnReturn = BaseObjectPoolConfig.DEFAULT_TEST_ON_RETURN;
        private volatile bool testWhileIdle = BaseObjectPoolConfig.DEFAULT_TEST_WHILE_IDLE;
        private AtomicLong timeBetweenEvictionRunsMillis = new AtomicLong(BaseObjectPoolConfig.DEFAULT_TIME_BETWEEN_EVICTION_RUNS_MILLIS);
        private volatile int numTestsPerEvictionRun = BaseObjectPoolConfig.DEFAULT_NUM_TESTS_PER_EVICTION_RUN;
        private AtomicLong minEvictableIdleTimeMillis = new AtomicLong(BaseObjectPoolConfig.DEFAULT_MIN_EVICTABLE_IDLE_TIME_MILLIS);
        private AtomicLong softMinEvictableIdleTimeMillis = new AtomicLong(BaseObjectPoolConfig.DEFAULT_SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS);
        private volatile EvictionPolicy<T> evictionPolicy;


        // Internal (primarily state) attributes
        internal readonly object closeLock = new object();
        internal volatile bool closed = false;
        internal readonly object evictionLock = new object();
        private Evictor evictor = null; // @GuardedBy("evictionLock")
        internal java.util.Iterator evictionIterator = null; // @GuardedBy("evictionLock")
        /*
         * Class loader for evictor thread to use since in a J2EE or similar
         * environment the context class loader for the evictor thread may have
         * visibility of the correct factory. See POOL-161.
         */
        private readonly java.lang.ClassLoader factoryClassLoader;

        // Monitoring (primarily JMX) attributes
        private readonly javax.management.ObjectName oname;
        private readonly string creationStackTrace;
        private readonly AtomicLong borrowedCount = new AtomicLong(0);
        private readonly AtomicLong returnedCount = new AtomicLong(0);
        internal readonly AtomicLong createdCount = new AtomicLong(0);
        internal readonly AtomicLong destroyedCount = new AtomicLong(0);
        internal readonly AtomicLong destroyedByEvictorCount = new AtomicLong(0);
        internal readonly AtomicLong destroyedByBorrowValidationCount = new AtomicLong(0);
        private readonly LinkedList activeTimes = new LinkedList(); // @GuardedBy("activeTimes") - except in initStats()
        private readonly LinkedList idleTimes = new LinkedList(); // @GuardedBy("activeTimes") - except in initStats()
        private readonly LinkedList waitTimes = new LinkedList(); // @GuardedBy("activeTimes") - except in initStats()
        private readonly object maxBorrowWaitTimeMillisLock = new object();
        private AtomicLong maxBorrowWaitTimeMillis = new AtomicLong(0); // @GuardedBy("maxBorrowWaitTimeMillisLock")
        private SwallowedExceptionListener swallowedExceptionListener = null;

        /// <summary>
        /// Handles JMX registration (if required) and the initialization required for
        /// monitoring.
        /// </summary>
        /// <param name="config">        Pool configuration </param>
        /// <param name="jmxNameBase">   The default base JMX name for the new pool unless
        ///                      overridden by the config </param>
        /// <param name="jmxNamePrefix"> Prefix to be used for JMX name for the new pool </param>
        public BaseGenericObjectPool(BaseObjectPoolConfig config, string jmxNameBase, string jmxNamePrefix)
        {
            if (config.JmxEnabled)
            {
                this.oname = jmxRegister(config, jmxNameBase, jmxNamePrefix);
            }
            else
            {
                this.oname = null;
            }

            // Populate the creation stack trace
            this.creationStackTrace = getStackTrace(new Exception());

            // save the current CCL to be used later by the evictor Thread
            factoryClassLoader = java.lang.Thread.currentThread().getContextClassLoader();

            // Initialise the attributes used to record rolling averages
            initStats();
        }


        /// <summary>
        /// Returns the maximum number of objects that can be allocated by the pool
        /// (checked out to clients, or idle awaiting checkout) at a given time. When
        /// negative, there is no limit to the number of objects that can be
        /// managed by the pool at one time.
        /// </summary>
        /// <returns> the cap on the total number of object instances managed by the
        ///         pool.
        /// </returns>
        /// <seealso cref= #setMaxTotal </seealso>
        public int MaxTotal
        {
            get
            {
                return maxTotal;
            }
            set
            {
                this.maxTotal = value;
            }
        }


        /// <summary>
        /// Returns whether to block when the <code>borrowObject()</code> method is
        /// invoked when the pool is exhausted (the maximum number of "active"
        /// objects has been reached).
        /// </summary>
        /// <returns> <code>true</code> if <code>borrowObject()</code> should block
        ///         when the pool is exhausted
        /// </returns>
        /// <seealso cref= #setBlockWhenExhausted </seealso>
        public bool BlockWhenExhausted
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
        /// Returns the maximum amount of time (in milliseconds) the
        /// <code>borrowObject()</code> method should block before throwing an
        /// exception when the pool is exhausted and
        /// <seealso cref="#getBlockWhenExhausted"/> is true. When less than 0, the
        /// <code>borrowObject()</code> method may block indefinitely.
        /// </summary>
        /// <returns> the maximum number of milliseconds <code>borrowObject()</code>
        ///         will block.
        /// </returns>
        /// <seealso cref= #setMaxWaitMillis </seealso>
        /// <seealso cref= #setBlockWhenExhausted </seealso>
        public long MaxWaitMillis
        {
            get
            {
                return maxWaitMillis.get();
            }
            set
            {
                this.maxWaitMillis.set(value);
            }
        }


        /// <summary>
        /// Returns whether the pool has LIFO (last in, first out) behaviour with
        /// respect to idle objects - always returning the most recently used object
        /// from the pool, or as a FIFO (first in, first out) queue, where the pool
        /// always returns the oldest object in the idle object pool.
        /// </summary>
        /// <returns> <code>true</code> if the pool is configured with LIFO behaviour
        ///         or <code>false</code> if the pool is configured with FIFO
        ///         behaviour
        /// </returns>
        /// <seealso cref= #setLifo </seealso>
        public bool Lifo
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
        /// Returns whether objects borrowed from the pool will be validated before
        /// being returned from the <code>borrowObject()</code> method. Validation is
        /// performed by the <code>validateObject()</code> method of the factory
        /// associated with the pool. If the object fails to validate, it will be
        /// removed from the pool and destroyed, and a new attempt will be made to
        /// borrow an object from the pool.
        /// </summary>
        /// <returns> <code>true</code> if objects are validated before being returned
        ///         from the <code>borrowObject()</code> method
        /// </returns>
        /// <seealso cref= #setTestOnBorrow </seealso>
        public bool TestOnBorrow
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
        /// Returns whether objects borrowed from the pool will be validated when
        /// they are returned to the pool via the <code>returnObject()</code> method.
        /// Validation is performed by the <code>validateObject()</code> method of
        /// the factory associated with the pool. Returning objects that fail validation
        /// are destroyed rather then being returned the pool.
        /// </summary>
        /// <returns> <code>true</code> if objects are validated on return to
        ///         the pool via the <code>returnObject()</code> method
        /// </returns>
        /// <seealso cref= #setTestOnReturn </seealso>
        public bool TestOnReturn
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
        /// Returns whether objects sitting idle in the pool will be validated by the
        /// idle object evictor (if any - see
        /// <seealso cref="#setTimeBetweenEvictionRunsMillis(long)"/>). Validation is performed
        /// by the <code>validateObject()</code> method of the factory associated
        /// with the pool. If the object fails to validate, it will be removed from
        /// the pool and destroyed.
        /// </summary>
        /// <returns> <code>true</code> if objects will be validated by the evictor
        /// </returns>
        /// <seealso cref= #setTestWhileIdle </seealso>
        /// <seealso cref= #setTimeBetweenEvictionRunsMillis </seealso>
        public bool TestWhileIdle
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
        /// Returns the number of milliseconds to sleep between runs of the idle
        /// object evictor thread. When non-positive, no idle object evictor thread
        /// will be run.
        /// </summary>
        /// <returns> number of milliseconds to sleep between evictor runs
        /// </returns>
        /// <seealso cref= #setTimeBetweenEvictionRunsMillis </seealso>
        public long TimeBetweenEvictionRunsMillis
        {
            get
            {
                return timeBetweenEvictionRunsMillis.get();
            }
            set
            {
                this.timeBetweenEvictionRunsMillis.set(value);
                startEvictor(value);
            }
        }


        /// <summary>
        /// Returns the maximum number of objects to examine during each run (if any)
        /// of the idle object evictor thread. When positive, the number of tests
        /// performed for a run will be the minimum of the configured value and the
        /// number of idle instances in the pool. When negative, the number of tests
        /// performed will be <code>ceil(<seealso cref="#getNumIdle"/>/
        /// abs(<seealso cref="#getNumTestsPerEvictionRun"/>)) which means that when the value
        /// is <code>-n</code> roughly one nth of the idle objects will be tested per
        /// run.
        /// </summary>
        /// <returns> max number of objects to examine during each evictor run
        /// </returns>
        /// <seealso cref= #setNumTestsPerEvictionRun </seealso>
        /// <seealso cref= #setTimeBetweenEvictionRunsMillis </seealso>
        public int NumTestsPerEvictionRun
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
        /// Returns the minimum amount of time an object may sit idle in the pool
        /// before it is eligible for eviction by the idle object evictor (if any -
        /// see <seealso cref="#setTimeBetweenEvictionRunsMillis(long)"/>). When non-positive,
        /// no objects will be evicted from the pool due to idle time alone.
        /// </summary>
        /// <returns> minimum amount of time an object may sit idle in the pool before
        ///         it is eligible for eviction
        /// </returns>
        /// <seealso cref= #setMinEvictableIdleTimeMillis </seealso>
        /// <seealso cref= #setTimeBetweenEvictionRunsMillis </seealso>
        public long MinEvictableIdleTimeMillis
        {
            get
            {
                return minEvictableIdleTimeMillis.get();
            }
            set
            {
                this.minEvictableIdleTimeMillis.set(value);
            }
        }


        /// <summary>
        /// Returns the minimum amount of time an object may sit idle in the pool
        /// before it is eligible for eviction by the idle object evictor (if any -
        /// see <seealso cref="#setTimeBetweenEvictionRunsMillis(long)"/>),
        /// with the extra condition that at least <code>minIdle</code> object
        /// instances remain in the pool. This setting is overridden by
        /// <seealso cref="#getMinEvictableIdleTimeMillis"/> (that is, if
        /// <seealso cref="#getMinEvictableIdleTimeMillis"/> is positive, then
        /// <seealso cref="#getSoftMinEvictableIdleTimeMillis"/> is ignored).
        /// </summary>
        /// <returns> minimum amount of time an object may sit idle in the pool before
        ///         it is eligible for eviction if minIdle instances are available
        /// </returns>
        /// <seealso cref= #setSoftMinEvictableIdleTimeMillis </seealso>
        public long SoftMinEvictableIdleTimeMillis
        {
            get
            {
                return softMinEvictableIdleTimeMillis.get();
            }
            set
            {
                this.softMinEvictableIdleTimeMillis.set(value);
            }
        }


        /// <summary>
        /// Returns the name of the <seealso cref="EvictionPolicy"/> implementation that is
        /// used by this pool.
        /// </summary>
        /// <returns>  The fully qualified class name of the <seealso cref="EvictionPolicy"/>
        /// </returns>
        /// <seealso cref= #setEvictionPolicyClassName(String) </seealso>
        public string EvictionPolicyClassName
        {
            get
            {
                return evictionPolicy.GetType().FullName;
            }
            set
            {
                this.evictionPolicy = new DefaultEvictionPolicy<T>();
            }
        }



        /// <summary>
        /// Closes the pool, destroys the remaining idle objects and, if registered
        /// in JMX, deregisters it.
        /// </summary>
        public abstract void close();

        /// <summary>
        /// Has this pool instance been closed. </summary>
        /// <returns> <code>true</code> when this pool has been closed. </returns>
        public bool Closed
        {
            get
            {
                return closed;
            }
        }

        /// <summary>
        /// <para>Perform <code>numTests</code> idle object eviction tests, evicting
        /// examined objects that meet the criteria for eviction. If
        /// <code>testWhileIdle</code> is true, examined objects are validated
        /// when visited (and removed if invalid); otherwise only objects that
        /// have been idle for more than <code>minEvicableIdleTimeMillis</code>
        /// are removed.</para>
        /// </summary>
        /// <exception cref="Exception"> when there is a problem evicting idle objects. </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public abstract void evict() throws Exception;
        public abstract void evict();

        /// <summary>
        /// Returns the <seealso cref="EvictionPolicy"/> defined for this pool. </summary>
        /// <returns> the eviction policy </returns>
        internal EvictionPolicy<T> EvictionPolicy
        {
            get
            {
                return evictionPolicy;
            }
        }

        /// <summary>
        /// Verifies that the pool is open. </summary>
        /// <exception cref="IllegalStateException"> if the pool is closed. </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: final void assertOpen() throws IllegalStateException
        internal void assertOpen()
        {
            if (Closed)
            {
                throw new java.lang.IllegalStateException("Pool not open");
            }
        }

        /// <summary>
        /// <para>Starts the evictor with the given delay. If there is an evictor
        /// running when this method is called, it is stopped and replaced with a
        /// new evictor with the specified delay.</para>
        /// 
        /// <para>This method needs to be final, since it is called from a constructor.
        /// See POOL-195.</para>
        /// </summary>
        /// <param name="delay"> time in milliseconds before start and between eviction runs </param>
        internal void startEvictor(long delay)
        {
            lock (evictionLock)
            {
                if (null != evictor)
                {
                    EvictionTimer.cancel(evictor);
                    evictor = null;
                    evictionIterator = null;
                }
                if (delay > 0)
                {
                    evictor = new Evictor(this);
                    EvictionTimer.schedule(evictor, delay, delay);
                }
            }
        }

        /// <summary>
        /// Tries to ensure that the configured minimum number of idle instances are
        /// available in the pool. </summary>
        /// <exception cref="Exception"> if an error occurs creating idle instances </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: abstract void ensureMinIdle() throws Exception;
        internal abstract void ensureMinIdle();


        // Monitoring (primarily JMX) related methods

        /// <summary>
        /// Provides the name under which the pool has been registered with the
        /// platform MBean server or <code>null</code> if the pool has not been
        /// registered. </summary>
        /// <returns> the JMX name </returns>
        public javax.management.ObjectName JmxName
        {
            get
            {
                return oname;
            }
        }

        /// <summary>
        /// Provides the stack trace for the call that created this pool. JMX
        /// registration may trigger a memory leak so it is important that pools are
        /// deregistered when no longer used by calling the <seealso cref="#close()"/> method.
        /// This method is provided to assist with identifying code that creates but
        /// does not close it thereby creating a memory leak. </summary>
        /// <returns> pool creation stack trace </returns>
        public string CreationStackTrace
        {
            get
            {
                return creationStackTrace;
            }
        }

        /// <summary>
        /// The total number of objects successfully borrowed from this pool over the
        /// lifetime of the pool. </summary>
        /// <returns> the borrowed object count </returns>
        public long BorrowedCount
        {
            get
            {
                return borrowedCount.get();
            }
        }

        /// <summary>
        /// The total number of objects returned to this pool over the lifetime of
        /// the pool. This excludes attempts to return the same object multiple
        /// times. </summary>
        /// <returns> the returned object count </returns>
        public long ReturnedCount
        {
            get
            {
                return returnedCount.get();
            }
        }

        /// <summary>
        /// The total number of objects created for this pool over the lifetime of
        /// the pool. </summary>
        /// <returns> the created object count </returns>
        public long CreatedCount
        {
            get
            {
                return createdCount.get();
            }
        }

        /// <summary>
        /// The total number of objects destroyed by this pool over the lifetime of
        /// the pool. </summary>
        /// <returns> the destroyed object count </returns>
        public long DestroyedCount
        {
            get
            {
                return destroyedCount.get();
            }
        }

        /// <summary>
        /// The total number of objects destroyed by the evictor associated with this
        /// pool over the lifetime of the pool. </summary>
        /// <returns> the evictor destroyed object count </returns>
        public long DestroyedByEvictorCount
        {
            get
            {
                return destroyedByEvictorCount.get();
            }
        }

        /// <summary>
        /// The total number of objects destroyed by this pool as a result of failing
        /// validation during <code>borrowObject()</code> over the lifetime of the
        /// pool. </summary>
        /// <returns> validation destroyed object count </returns>
        public long DestroyedByBorrowValidationCount
        {
            get
            {
                return destroyedByBorrowValidationCount.get();
            }
        }

        /// <summary>
        /// The mean time objects are active for based on the last {@link
        /// #MEAN_TIMING_STATS_CACHE_SIZE} objects returned to the pool. </summary>
        /// <returns> mean time an object has been checked out from the pool among
        /// recently returned objects </returns>
        public long MeanActiveTimeMillis
        {
            get
            {
                return getMeanFromStatsCache(activeTimes);
            }
        }

        /// <summary>
        /// The mean time objects are idle for based on the last {@link
        /// #MEAN_TIMING_STATS_CACHE_SIZE} objects borrowed from the pool. </summary>
        /// <returns> mean time an object has been idle in the pool among recently
        /// borrowed objects </returns>
        public long MeanIdleTimeMillis
        {
            get
            {
                return getMeanFromStatsCache(idleTimes);
            }
        }

        /// <summary>
        /// The mean time threads wait to borrow an object based on the last {@link
        /// #MEAN_TIMING_STATS_CACHE_SIZE} objects borrowed from the pool. </summary>
        /// <returns> mean time in milliseconds that a recently served thread has had
        /// to wait to borrow an object from the pool </returns>
        public long MeanBorrowWaitTimeMillis
        {
            get
            {
                return getMeanFromStatsCache(waitTimes);
            }
        }

        /// <summary>
        /// The maximum time a thread has waited to borrow objects from the pool. </summary>
        /// <returns> maximum wait time in milliseconds since the pool was created </returns>
        public long MaxBorrowWaitTimeMillis
        {
            get
            {
                return maxBorrowWaitTimeMillis.get();
            }
        }

        /// <summary>
        /// The number of instances currently idle in this pool. </summary>
        /// <returns> count of instances available for checkout from the pool </returns>
        public abstract int NumIdle {get;}

        /// <summary>
        /// The listener used (if any) to receive notifications of exceptions
        /// unavoidably swallowed by the pool.
        /// </summary>
        /// <returns> The listener or <code>null</code> for no listener </returns>
        public SwallowedExceptionListener SwallowedExceptionListener
        {
            get
            {
                return swallowedExceptionListener;
            }
            set
            {
                this.swallowedExceptionListener = value;
            }
        }


        /// <summary>
        /// Swallows an exception and notifies the configured listener for swallowed
        /// exceptions queue.
        /// </summary>
        /// <param name="e"> exception to be swallowed </param>
        internal void swallowException(Exception e)
        {
            SwallowedExceptionListener listener = SwallowedExceptionListener;

            if (listener == null)
            {
                return;
            }

            try
            {
                listener.onSwallowException(e);
            }
            catch (java.lang.VirtualMachineError vme)
            {
                throw vme;
            }
            catch (Exception)
            {
                // Ignore. Enjoy the irony.
            }
        }

        /// <summary>
        /// Updates statistics after an object is borrowed from the pool. </summary>
        /// <param name="p"> object borrowed from the pool </param>
        /// <param name="waitTime"> time (in milliseconds) that the borrowing thread had to wait </param>
        internal void updateStatsBorrow(PooledObject<T> p, long waitTime)
        {
            borrowedCount.incrementAndGet();
            lock (idleTimes)
            {
                idleTimes.addLast(java.lang.Long.valueOf(p.IdleTimeMillis));
                idleTimes.removeFirst();
            }
            lock (waitTimes)
            {
                waitTimes.addLast(java.lang.Long.valueOf(waitTime));
                waitTimes.removeFirst();
            }
            lock (maxBorrowWaitTimeMillisLock)
            {
                if (waitTime > maxBorrowWaitTimeMillis.get())
                {
                    maxBorrowWaitTimeMillis.set(waitTime);
                }
            }
        }

        /// <summary>
        /// Updates statistics after an object is returned to the pool. </summary>
        /// <param name="activeTime"> the amount of time (in milliseconds) that the returning
        /// object was checked out </param>
        internal void updateStatsReturn(long activeTime)
        {
            returnedCount.incrementAndGet();
            lock (activeTimes)
            {
                activeTimes.addLast(java.lang.Long.valueOf(activeTime));
                activeTimes.removeFirst();
            }
        }

        /// <summary>
        /// Unregisters this pool's MBean.
        /// </summary>
        internal void jmxUnregister()
        {
            if (oname != null)
            {
                try
                {
                    java.lang.management.ManagementFactory.getPlatformMBeanServer().unregisterMBean(oname);
                }
                catch (MBeanRegistrationException e)
                {
                    swallowException(e);
                }
                catch (InstanceNotFoundException e)
                {
                    swallowException(e);
                }
            }
        }

        /// <summary>
        /// Registers the pool with the platform MBean server.
        /// The registered name will be
        /// <code>jmxNameBase + jmxNamePrefix + i</code> where i is the least
        /// integer greater than or equal to 1 such that the name is not already
        /// registered. Swallows MBeanRegistrationException, NotCompliantMBeanException
        /// returning null.
        /// </summary>
        /// <param name="config"> Pool configuration </param>
        /// <param name="jmxNameBase"> default base JMX name for this pool </param>
        /// <param name="jmxNamePrefix"> name prefix </param>
        /// <returns> registered ObjectName, null if registration fails </returns>
        private ObjectName jmxRegister(BaseObjectPoolConfig config, string jmxNameBase, string jmxNamePrefix)
        {
            ObjectName objectName = null;
            MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
            int i = 1;
            bool registered = false;
            string @base = config.JmxNameBase;
            if (@base == null)
            {
                @base = jmxNameBase;
            }
            while (!registered)
            {
                try
                {
                    ObjectName objName;
                    // Skip the numeric suffix for the first pool in case there is
                    // only one so the names are cleaner.
                    if (i == 1)
                    {
                        objName = new ObjectName(@base + jmxNamePrefix);
                    }
                    else
                    {
                        objName = new ObjectName(@base + jmxNamePrefix + i);
                    }
                    mbs.registerMBean(this, objName);
                    objectName = objName;
                    registered = true;
                }
                catch (MalformedObjectNameException)
                {
                    if (BaseObjectPoolConfig.DEFAULT_JMX_NAME_PREFIX.Equals(jmxNamePrefix) && jmxNameBase.Equals(@base))
                    {
                        // Shouldn't happen. Skip registration if it does.
                        registered = true;
                    }
                    else
                    {
                        // Must be an invalid name. Use the defaults instead.
                        jmxNamePrefix = BaseObjectPoolConfig.DEFAULT_JMX_NAME_PREFIX;
                        @base = jmxNameBase;
                    }
                }
                catch (InstanceAlreadyExistsException)
                {
                    // Increment the index and try again
                    i++;
                }
                catch (MBeanRegistrationException)
                {
                    // Shouldn't happen. Skip registration if it does.
                    registered = true;
                }
                catch (NotCompliantMBeanException)
                {
                    // Shouldn't happen. Skip registration if it does.
                    registered = true;
                }
            }
            return objectName;
        }

        /// <summary>
        /// Gets the stack trace of an exception as a string. </summary>
        /// <param name="e"> exception to trace </param>
        /// <returns> exception stack trace as a string </returns>
        private string getStackTrace(Exception e)
        {
            // Need the exception in string form to prevent the retention of
            // references to classes in the stack trace that could trigger a memory
            // leak in a container environment.
            return e.StackTrace;
        }

        /// <summary>
        /// Returns the greatest integer less than ore equal to the arithmetic mean
        /// of the entries in <code>cache,</code> acquiring and holding the argument's
        /// monitor while making a local copy. </summary>
        /// <param name="cache"> list containing entries to analyze </param>
        /// <returns> truncated arithmetic mean </returns>
        private long getMeanFromStatsCache(LinkedList cache)
        {
            List times = new ArrayList(MEAN_TIMING_STATS_CACHE_SIZE);
            lock (cache)
            {
                times.addAll(cache);
            }
            double result = 0;
            int counter = 0;
            Iterator iter = times.iterator();
            while (iter.hasNext())
            {
                java.lang.Long time = (java.lang.Long)iter.next();
                if (time != null)
                {
                    counter++;
                    result = result * ((counter - 1) / (double) counter) + time.longValue() / (double) counter;
                }
            }
            return (long) result;
        }

        /// <summary>
        /// Initializes pool statistics.
        /// </summary>
        private void initStats()
        {
            for (int i = 0; i < MEAN_TIMING_STATS_CACHE_SIZE; i++)
            {
                activeTimes.addLast(null);
                idleTimes.addLast(null);
                waitTimes.addLast(null);
            }
        }


        // Inner classes

        /// <summary>
        /// The idle object evictor <seealso cref="TimerTask"/>.
        /// </summary>
        /// <seealso cref= GenericKeyedObjectPool#setTimeBetweenEvictionRunsMillis </seealso>
        internal class Evictor : TimerTask
        {
            private readonly BaseGenericObjectPool<T> outerInstance;

            public Evictor(BaseGenericObjectPool<T> outerInstance)
            {
                this.outerInstance = outerInstance;
            }

            /// <summary>
            /// Run pool maintenance.  Evict objects qualifying for eviction and then
            /// ensure that the minimum number of idle instances are available.
            /// Since the Timer that invokes Evictors is shared for all Pools but
            /// pools may exist in different class loaders, the Evictor ensures that
            /// any actions taken are under the class loader of the factory
            /// associated with the pool.
            /// </summary>
            public override void run()
            {
                java.lang.ClassLoader savedClassLoader = java.lang.Thread.currentThread().getContextClassLoader();
                try
                {
                    // Set the class loader for the factory
                    java.lang.Thread.currentThread().setContextClassLoader(outerInstance.factoryClassLoader);

                    // Evict from the pool
                    try
                    {
                        outerInstance.evict();
                    }
                    catch (System.OutOfMemoryException oome)
                    {
                        // Log problem but give evictor thread a chance to continue
                        // in case error is recoverable
                        java.lang.System.err.write(System.Text.Encoding.UTF8.GetBytes(oome.StackTrace));
                    }
                    catch (Exception e)
                    {
                        outerInstance.swallowException(e);
                    }

                    // Re-create idle instances.
                    try
                    {
                        outerInstance.ensureMinIdle();
                    }
                    catch (Exception e)
                    {
                        outerInstance.swallowException(e);
                    }
                }
                finally
                {
                    // Restore the previous CCL
                    java.lang.Thread.currentThread().setContextClassLoader(savedClassLoader);
                }
            }
        }
    }

}