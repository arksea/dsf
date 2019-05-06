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

    /// <summary>
    /// A configurable <seealso cref="ObjectPool"/> implementation.
    /// <para>
    /// When coupled with the appropriate <seealso cref="PooledObjectFactory"/>,
    /// <code>GenericObjectPool</code> provides robust pooling functionality for
    /// arbitrary objects.</para>
    /// <para>
    /// Optionally, one may configure the pool to examine and possibly evict objects
    /// as they sit idle in the pool and to ensure that a minimum number of idle
    /// objects are available. This is performed by an "idle object eviction" thread,
    /// which runs asynchronously. Caution should be used when configuring this
    /// optional feature. Eviction runs contend with client threads for access to
    /// objects in the pool, so if they run too frequently performance issues may
    /// result.</para>
    /// <para>
    /// The pool can also be configured to detect and remove "abandoned" objects,
    /// i.e. objects that have been checked out of the pool but neither used nor
    /// returned before the configured
    /// <seealso cref="AbandonedConfig#getRemoveAbandonedTimeout() removeAbandonedTimeout"/>.
    /// Abandoned object removal can be configured to happen when
    /// <code>borrowObject</code> is invoked and the pool is close to starvation, or
    /// it can be executed by the idle object evictor, or both. If pooled objects
    /// implement the <seealso cref="TrackedUse"/> interface, their last use will be queried
    /// using the <code>getLastUsed</code> method on that interface; otherwise
    /// abandonment is determined by how long an object has been checked out from
    /// the pool.</para>
    /// <para>
    /// Implementation note: To prevent possible deadlocks, care has been taken to
    /// ensure that no call to a factory method will occur within a synchronization
    /// block. See POOL-125 and DBCP-44 for more information.</para>
    /// <para>
    /// This class is intended to be thread-safe.</para>
    /// </summary>
    /// <seealso cref= GenericKeyedObjectPool
    /// </seealso>
    /// @param <T> Type of element pooled in this pool.
    /// 
    /// @version $Revision: 1545920 $
    /// 
    /// @since 2.0 </param>
    public class GenericObjectPool<T> : BaseGenericObjectPool<T>, ObjectPool<T>, UsageTracking<T>
    {

        /// <summary>
        /// Create a new <code>GenericObjectPool</code> using defaults from
        /// <seealso cref="GenericObjectPoolConfig"/>.
        /// </summary>
        /// <param name="factory"> The object factory to be used to create object instances
        ///                used by this pool </param>
        public GenericObjectPool(PooledObjectFactory<T> factory) : this(factory, new GenericObjectPoolConfig())
        {
        }

        /// <summary>
        /// Create a new <code>GenericObjectPool</code> using a specific
        /// configuration.
        /// </summary>
        /// <param name="factory">   The object factory to be used to create object instances
        ///                  used by this pool </param>
        /// <param name="config">    The configuration to use for this pool instance. The
        ///                  configuration is used by value. Subsequent changes to
        ///                  the configuration object will not be reflected in the
        ///                  pool. </param>
        public GenericObjectPool(PooledObjectFactory<T> factory, GenericObjectPoolConfig config) : base(config, ONAME_BASE, config.JmxNamePrefix)
        {


            if (factory == null)
            {
                jmxUnregister(); // tidy up
                throw new System.ArgumentException("factory may not be null");
            }
            this.factory = factory;

            Config = config;

            startEvictor(TimeBetweenEvictionRunsMillis);
        }

        /// <summary>
        /// Create a new <code>GenericObjectPool</code> that tracks and destroys
        /// objects that are checked out, but never returned to the pool.
        /// </summary>
        /// <param name="factory">   The object factory to be used to create object instances
        ///                  used by this pool </param>
        /// <param name="config">    The base pool configuration to use for this pool instance.
        ///                  The configuration is used by value. Subsequent changes to
        ///                  the configuration object will not be reflected in the
        ///                  pool. </param>
        /// <param name="abandonedConfig">  Configuration for abandoned object identification
        ///                         and removal.  The configuration is used by value. </param>
        public GenericObjectPool(PooledObjectFactory<T> factory, GenericObjectPoolConfig config, AbandonedConfig abandonedConfig) : this(factory, config)
        {
            setAbandonedConfig(abandonedConfig);
        }

        /// <summary>
        /// Returns the cap on the number of "idle" instances in the pool. If maxIdle
        /// is set too low on heavily loaded systems it is possible you will see
        /// objects being destroyed and almost immediately new objects being created.
        /// This is a result of the active threads momentarily returning objects
        /// faster than they are requesting them them, causing the number of idle
        /// objects to rise above maxIdle. The best value for maxIdle for heavily
        /// loaded system will vary but the default is a good starting point.
        /// </summary>
        /// <returns> the maximum number of "idle" instances that can be held in the
        ///         pool or a negative value if there is no limit
        /// </returns>
        /// <seealso cref= #setMaxIdle </seealso>
        public int MaxIdle
        {
            get
            {
                return maxIdle;
            }
            set
            {
                this.maxIdle = value;
            }
        }


        /// <summary>
        /// Sets the target for the minimum number of idle objects to maintain in
        /// the pool. This setting only has an effect if it is positive and
        /// <seealso cref="#getTimeBetweenEvictionRunsMillis()"/> is greater than zero. If this
        /// is the case, an attempt is made to ensure that the pool has the required
        /// minimum number of instances during idle object eviction runs.
        /// <para>
        /// If the configured value of minIdle is greater than the configured value
        /// for maxIdle then the value of maxIdle will be used instead.
        /// 
        /// </para>
        /// </summary>
        /// <param name="minIdle">
        ///            The minimum number of objects.
        /// </param>
        /// <seealso cref= #getMinIdle() </seealso>
        /// <seealso cref= #getMaxIdle() </seealso>
        /// <seealso cref= #getTimeBetweenEvictionRunsMillis() </seealso>
        public virtual int MinIdle
        {
            set
            {
                this.minIdle = value;
            }
            get
            {
                int maxIdleSave = MaxIdle;
                if (this.minIdle > maxIdleSave)
                {
                    return maxIdleSave;
                }
                else
                {
                    return minIdle;
                }
            }
        }


        /// <summary>
        /// Whether or not abandoned object removal is configured for this pool.
        /// </summary>
        /// <returns> true if this pool is configured to detect and remove
        /// abandoned objects </returns>
        public bool isAbandonedConfig()
        {
            return abandonedConfig != null;
        }

        /// <summary>
        /// Will this pool identify and log any abandoned objects?
        /// </summary>
        /// <returns> {@code true} if abandoned object removal is configured for this
        ///         pool and removal events are to be logged otherwise {@code false}
        /// </returns>
        /// <seealso cref= AbandonedConfig#getLogAbandoned() </seealso>
        public bool LogAbandoned
        {
            get
            {
                AbandonedConfig ac = this.abandonedConfig;
                return ac != null && ac.LogAbandoned;
            }
        }

        /// <summary>
        /// Will a check be made for abandoned objects when an object is borrowed
        /// from this pool?
        /// </summary>
        /// <returns> {@code true} if abandoned object removal is configured to be
        ///         activated by borrowObject otherwise {@code false}
        /// </returns>
        /// <seealso cref= AbandonedConfig#getRemoveAbandonedOnBorrow() </seealso>
        public bool RemoveAbandonedOnBorrow
        {
            get
            {
                AbandonedConfig ac = this.abandonedConfig;
                return ac != null && ac.RemoveAbandonedOnBorrow;
            }
        }

        /// <summary>
        /// Will a check be made for abandoned objects when the evictor runs?
        /// </summary>
        /// <returns> {@code true} if abandoned object removal is configured to be
        ///         activated when the evictor runs otherwise {@code false}
        /// </returns>
        /// <seealso cref= AbandonedConfig#getRemoveAbandonedOnMaintenance() </seealso>
        public bool RemoveAbandonedOnMaintenance
        {
            get
            {
                AbandonedConfig ac = this.abandonedConfig;
                return ac != null && ac.RemoveAbandonedOnMaintenance;
            }
        }

        /// <summary>
        /// Obtain the timeout before which an object will be considered to be
        /// abandoned by this pool.
        /// </summary>
        /// <returns> The abandoned object timeout in seconds if abandoned object
        ///         removal is configured for this pool; Integer.MAX_VALUE otherwise.
        /// </returns>
        /// <seealso cref= AbandonedConfig#getRemoveAbandonedTimeout() </seealso>
        public int RemoveAbandonedTimeout
        {
            get
            {
                AbandonedConfig ac = this.abandonedConfig;
                return ac != null ? ac.RemoveAbandonedTimeout : int.MaxValue;
            }
        }


        /// <summary>
        /// Sets the base pool configuration.
        /// </summary>
        /// <param name="conf"> the new configuration to use. This is used by value.
        /// </param>
        /// <seealso cref= GenericObjectPoolConfig </seealso>
        public virtual GenericObjectPoolConfig Config
        {
            set
            {
                Lifo = value.Lifo;
                MaxIdle = value.MaxIdle;
                MinIdle = value.MinIdle;
                MaxTotal = value.MaxTotal;
                MaxWaitMillis = value.MaxWaitMillis;
                BlockWhenExhausted = value.BlockWhenExhausted;
                TestOnBorrow = value.TestOnBorrow;
                TestOnReturn = value.TestOnReturn;
                TestWhileIdle = value.TestWhileIdle;
                NumTestsPerEvictionRun = value.NumTestsPerEvictionRun;
                MinEvictableIdleTimeMillis = value.MinEvictableIdleTimeMillis;
                TimeBetweenEvictionRunsMillis = value.TimeBetweenEvictionRunsMillis;
                SoftMinEvictableIdleTimeMillis = value.SoftMinEvictableIdleTimeMillis;
                EvictionPolicyClassName = value.EvictionPolicyClassName;
            }
        }

        /// <summary>
        /// Sets the abandoned object removal configuration.
        /// </summary>
        /// <param name="abandonedConfig"> the new configuration to use. This is used by value.
        /// </param>
        /// <seealso cref= AbandonedConfig </seealso>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public void setAbandonedConfig(AbandonedConfig abandonedConfig) throws IllegalArgumentException
        public virtual void setAbandonedConfig(AbandonedConfig abandonedConfig)
        {
            if (abandonedConfig == null)
            {
                this.abandonedConfig = null;
            }
            else
            {
                this.abandonedConfig = new AbandonedConfig();
                this.abandonedConfig.LogAbandoned = abandonedConfig.LogAbandoned;
                this.abandonedConfig.LogWriter = abandonedConfig.LogWriter;
                this.abandonedConfig.RemoveAbandonedOnBorrow = abandonedConfig.RemoveAbandonedOnBorrow;
                this.abandonedConfig.RemoveAbandonedOnMaintenance = abandonedConfig.RemoveAbandonedOnMaintenance;
                this.abandonedConfig.RemoveAbandonedTimeout = abandonedConfig.RemoveAbandonedTimeout;
                this.abandonedConfig.UseUsageTracking = abandonedConfig.UseUsageTracking;
            }
        }

        /// <summary>
        /// Obtain a reference to the factory used to create, destroy and validate
        /// the objects used by this pool.
        /// </summary>
        /// <returns> the factory </returns>
        public virtual PooledObjectFactory<T> Factory
        {
            get
            {
                return factory;
            }
        }

        /// <summary>
        /// Equivalent to <code>{@link #borrowObject(long)
        /// borrowObject}(<seealso cref="#getMaxWaitMillis()"/>)</code>.
        /// <para>
        /// {@inheritDoc}
        /// </para>
        /// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public T borrowObject() throws Exception
        public T borrowObject()
        {
            return borrowObject(MaxWaitMillis);
        }

        /// <summary>
        /// Borrow an object from the pool using the specific waiting time which only
        /// applies if <seealso cref="#getBlockWhenExhausted()"/> is true.
        /// <para>
        /// If there is one or more idle instance available in the pool, then an
        /// idle instance will be selected based on the value of <seealso cref="#getLifo()"/>,
        /// activated and returned. If activation fails, or {@link #getTestOnBorrow()
        /// testOnBorrow} is set to <code>true</code> and validation fails, the
        /// instance is destroyed and the next available instance is examined. This
        /// continues until either a valid instance is returned or there are no more
        /// idle instances available.
        /// </para>
        /// <para>
        /// If there are no idle instances available in the pool, behavior depends on
        /// the <seealso cref="#getMaxTotal() maxTotal"/>, (if applicable)
        /// <seealso cref="#getBlockWhenExhausted()"/> and the value passed in to the
        /// <code>borrowMaxWaitMillis</code> parameter. If the number of instances
        /// checked out from the pool is less than <code>maxTotal,</code> a new
        /// instance is created, activated and (if applicable) validated and returned
        /// to the caller.
        /// </para>
        /// <para>
        /// If the pool is exhausted (no available idle instances and no capacity to
        /// create new ones), this method will either block (if
        /// <seealso cref="#getBlockWhenExhausted()"/> is true) or throw a
        /// <code>NoSuchElementException</code> (if
        /// <seealso cref="#getBlockWhenExhausted()"/> is false). The length of time that this
        /// method will block when <seealso cref="#getBlockWhenExhausted()"/> is true is
        /// determined by the value passed in to the <code>borrowMaxWaitMillis</code>
        /// parameter.
        /// </para>
        /// <para>
        /// When the pool is exhausted, multiple calling threads may be
        /// simultaneously blocked waiting for instances to become available. A
        /// "fairness" algorithm has been implemented to ensure that threads receive
        /// available instances in request arrival order.
        /// 
        /// </para>
        /// </summary>
        /// <param name="borrowMaxWaitMillis"> The time to wait in milliseconds for an object
        ///                            to become available
        /// </param>
        /// <returns> object instance from the pool
        /// </returns>
        /// <exception cref="NoSuchElementException"> if an instance cannot be returned </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public T borrowObject(long borrowMaxWaitMillis) throws Exception
        public virtual T borrowObject(long borrowMaxWaitMillis)
        {
            assertOpen();

            AbandonedConfig ac = this.abandonedConfig;
            if (ac != null && ac.RemoveAbandonedOnBorrow && (NumIdle < 2) && (NumActive > MaxTotal - 3))
            {
                removeAbandoned(ac);
            }

            PooledObject<T> p = null;

            // Get local copy of current config so it is consistent for entire
            // method execution
            bool blockWhenExhausted = BlockWhenExhausted;

            bool iscreate;
            long waitTime = 0;

            while (p == null)
            {
                iscreate = false;
                if (blockWhenExhausted)
                {
                    p = (PooledObject<T>)idleObjects.pollFirst();
                    if (p == null)
                    {
                        iscreate = true;
                        p = create();
                    }
                    if (p == null)
                    {
                        if (borrowMaxWaitMillis < 0)
                        {
                            p = idleObjects.takeFirst();
                        }
                        else
                        {
                            waitTime = java.lang.System.currentTimeMillis();
                            p = idleObjects.pollFirst(borrowMaxWaitMillis, java.util.concurrent.TimeUnit.MILLISECONDS);
                            waitTime = java.lang.System.currentTimeMillis() - waitTime;
                        }
                    }
                    if (p == null)
                    {
                        throw new NoSuchElementException("Timeout waiting for idle object");
                    }
                    if (!p.allocate())
                    {
                        p = null;
                    }
                }
                else
                {
                    p = (PooledObject<T>)idleObjects.pollFirst();
                    if (p == null)
                    {
                        iscreate = true;
                        p = create();
                    }
                    if (p == null)
                    {
                        throw new NoSuchElementException("Pool exhausted");
                    }
                    if (!p.allocate())
                    {
                        p = null;
                    }
                }

                if (p != null)
                {
                    try
                    {
                        factory.activateObject(p);
                    }
                    catch (Exception e)
                    {
                        try
                        {
                            destroy(p);
                        }
                        catch (Exception)
                        {
                            // Ignore - activation failure is more important
                        }
                        p = null;
                        if (iscreate)
                        {
                            NoSuchElementException nsee = new NoSuchElementException("Unable to activate object");
                            nsee.initCause(e);
                            throw nsee;
                        }
                    }
                    if (p != null && TestOnBorrow)
                    {
                        bool validate = false;
                        Exception validationThrowable = null;
                        try
                        {
                            validate = factory.validateObject(p);
                        }
                        catch (Exception t)
                        {
                            PoolUtils.checkRethrow(t);
                            validationThrowable = t;
                        }
                        if (!validate)
                        {
                            try
                            {
                                destroy(p);
                                destroyedByBorrowValidationCount.incrementAndGet();
                            }
                            catch (Exception)
                            {
                                // Ignore - validation failure is more important
                            }
                            p = null;
                            if (iscreate)
                            {
                                NoSuchElementException nsee = new NoSuchElementException("Unable to validate object");
                                nsee.initCause(validationThrowable);
                                throw nsee;
                            }
                        }
                    }
                }
            }

            updateStatsBorrow(p, waitTime);

            return p.Object;
        }

        /// <summary>
        /// Returns an object instance to the pool.
        /// <para>
        /// If <seealso cref="#getMaxIdle() maxIdle"/> is set to a positive value and the
        /// number of idle instances has reached this value, the returning instance
        /// is destroyed.
        /// </para>
        /// <para>
        /// If <seealso cref="#getTestOnReturn() testOnReturn"/> == true, the returning
        /// instance is validated before being returned to the idle instance pool. In
        /// this case, if validation fails, the instance is destroyed.
        /// </para>
        /// <para>
        /// Exceptions encountered destroying objects for any reason are swallowed
        /// but notified via a <seealso cref="SwallowedExceptionListener"/>.
        /// 
        /// </para>
        /// </summary>
        /// <param name="obj"> instance to return to the pool </param>
        public void returnObject(T obj)
        {
            PooledObject<T> p = (PooledObject<T>)allObjects.get(obj);

            if (!isAbandonedConfig())
            {
                if (p == null)
                {
                    throw new java.lang.IllegalStateException("Returned object not currently part of this pool");
                }
            }
            else
            {
                if (p == null)
                {
                    return; // Object was abandoned and removed
                }
                else
                {
                    // Make sure object is not being reclaimed
                    lock (p)
                    {
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final org.apache.commons.pool2.PooledObjectState state = p.getState();
                        PooledObjectState state = p.State;
                        if (state == PooledObjectState.ABANDONED || state == PooledObjectState.INVALID)
                        {
                            return;
                        }
                        else
                        {
                            p.markReturning(); // Keep from being marked abandoned
                        }
                    }
                }
            }

            long activeTime = p.ActiveTimeMillis;

            if (TestOnReturn)
            {
                if (!factory.validateObject(p))
                {
                    try
                    {
                        destroy(p);
                    }
                    catch (Exception e)
                    {
                        swallowException(e);
                    }
                    try
                    {
                        ensureIdle(1, false);
                    }
                    catch (Exception e)
                    {
                        swallowException(e);
                    }
                    updateStatsReturn(activeTime);
                    return;
                }
            }

            try
            {
                factory.passivateObject(p);
            }
            catch (Exception e1)
            {
                swallowException(e1);
                try
                {
                    destroy(p);
                }
                catch (Exception e)
                {
                    swallowException(e);
                }
                try
                {
                    ensureIdle(1, false);
                }
                catch (Exception e)
                {
                    swallowException(e);
                }
                updateStatsReturn(activeTime);
                return;
            }

            if (!p.deallocate())
            {
                throw new java.lang.IllegalStateException("Object has already been retured to this pool or is invalid");
            }

            int maxIdleSave = MaxIdle;
            if (Closed || maxIdleSave > -1 && maxIdleSave <= idleObjects.size())
            {
                try
                {
                    destroy(p);
                }
                catch (Exception e)
                {
                    swallowException(e);
                }
            }
            else
            {
                if (Lifo)
                {
                    idleObjects.addFirst(p);
                }
                else
                {
                    idleObjects.addLast(p);
                }
            }
            updateStatsReturn(activeTime);
        }

        /// <summary>
        /// {@inheritDoc}
        /// <para>
        /// Activation of this method decrements the active count and attempts to
        /// destroy the instance.
        /// 
        /// </para>
        /// </summary>
        /// <exception cref="Exception">             if an exception occurs destroying the
        ///                               object </exception>
        /// <exception cref="IllegalStateException"> if obj does not belong to this pool </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void invalidateObject(T obj) throws Exception
        public void invalidateObject(T obj)
        {
            PooledObject<T> p = (PooledObject<T>)allObjects.get(obj);
            if (p == null)
            {
                if (isAbandonedConfig())
                {
                    return;
                }
                else
                {
                    throw new java.lang.IllegalStateException("Invalidated object not currently part of this pool");
                }
            }
            lock (p)
            {
                if (p.State != PooledObjectState.INVALID)
                {
                    destroy(p);
                }
            }
            ensureIdle(1, false);
        }

        /// <summary>
        /// Clears any objects sitting idle in the pool by removing them from the
        /// idle instance pool and then invoking the configured
        /// <seealso cref="PooledObjectFactory#destroyObject(PooledObject)"/> method on each
        /// idle instance.
        /// <para>
        /// Implementation notes:
        /// <ul>
        /// <li>This method does not destroy or effect in any way instances that are
        /// checked out of the pool when it is invoked.</li>
        /// <li>Invoking this method does not prevent objects being returned to the
        /// idle instance pool, even during its execution. Additional instances may
        /// be returned while removed items are being destroyed.</li>
        /// <li>Exceptions encountered destroying idle instances are swallowed
        /// but notified via a <seealso cref="SwallowedExceptionListener"/>.</li>
        /// </ul>
        /// </para>
        /// </summary>
        public void clear()
        {
            PooledObject<T> p = (PooledObject<T>)idleObjects.poll();

            while (p != null)
            {
                try
                {
                    destroy(p);
                }
                catch (Exception e)
                {
                    swallowException(e);
                }
                p = (PooledObject<T>)idleObjects.poll();
            }
        }

        public int NumActive
        {
            get
            {
                return allObjects.size() - idleObjects.size();
            }
        }

        public override int NumIdle
        {
            get
            {
                return idleObjects.size();
            }
        }

        /// <summary>
        /// Closes the pool. Once the pool is closed, <seealso cref="#borrowObject()"/> will
        /// fail with IllegalStateException, but <seealso cref="#returnObject(Object)"/> and
        /// <seealso cref="#invalidateObject(Object)"/> will continue to work, with returned
        /// objects destroyed on return.
        /// <para>
        /// Destroys idle instances in the pool by invoking <seealso cref="#clear()"/>.
        /// </para>
        /// </summary>
        public override void close()
        {
            if (Closed)
            {
                return;
            }

            lock (closeLock)
            {
                if (Closed)
                {
                    return;
                }

                // Stop the evictor before the pool is closed since evict() calls
                // assertOpen()
                startEvictor(-1L);

                closed = true;
                // This clear removes any idle objects
                clear();

                jmxUnregister();

                // Release any threads that were waiting for an object
                idleObjects.interuptTakeWaiters();
            }
        }

        /// <summary>
        /// {@inheritDoc}
        /// <para>
        /// Successive activations of this method examine objects in sequence,
        /// cycling through objects in oldest-to-youngest order.
        /// </para>
        /// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void evict() throws Exception
        public override void evict()
        {
            assertOpen();

            if (idleObjects.size() > 0)
            {

                PooledObject<T> underTest = null;
                EvictionPolicy<T> evictionPolicy = EvictionPolicy;

                lock (evictionLock)
                {
                    EvictionConfig evictionConfig = new EvictionConfig(MinEvictableIdleTimeMillis, SoftMinEvictableIdleTimeMillis, MinIdle);

                    bool testWhileIdle = TestWhileIdle;

                    for (int i = 0, m = NumTests; i < m; i++)
                    {
//JAVA TO C# CONVERTER TODO TASK: Java iterators are only converted within the context of 'while' and 'for' loops:
                        if (evictionIterator == null || !evictionIterator.hasNext())
                        {
                            if (Lifo)
                            {
                                evictionIterator = idleObjects.descendingIterator();
                            }
                            else
                            {
                                evictionIterator = idleObjects.iterator();
                            }
                        }
//JAVA TO C# CONVERTER TODO TASK: Java iterators are only converted within the context of 'while' and 'for' loops:
                        if (!evictionIterator.hasNext())
                        {
                            // Pool exhausted, nothing to do here
                            return;
                        }

                        try
                        {
//JAVA TO C# CONVERTER TODO TASK: Java iterators are only converted within the context of 'while' and 'for' loops:
                            underTest = (PooledObject<T>)evictionIterator.next();
                        }
                        catch (NoSuchElementException)
                        {
                            // Object was borrowed in another thread
                            // Don't count this as an eviction test so reduce i;
                            i--;
                            evictionIterator = null;
                            continue;
                        }

                        if (!underTest.startEvictionTest())
                        {
                            // Object was borrowed in another thread
                            // Don't count this as an eviction test so reduce i;
                            i--;
                            continue;
                        }

                        if (evictionPolicy.evict(evictionConfig, underTest, idleObjects.size()))
                        {
                            destroy(underTest);
                            destroyedByEvictorCount.incrementAndGet();
                        }
                        else
                        {
                            if (testWhileIdle)
                            {
                                bool active = false;
                                try
                                {
                                    factory.activateObject(underTest);
                                    active = true;
                                }
                                catch (Exception)
                                {
                                    destroy(underTest);
                                    destroyedByEvictorCount.incrementAndGet();
                                }
                                if (active)
                                {
                                    if (!factory.validateObject(underTest))
                                    {
                                        destroy(underTest);
                                        destroyedByEvictorCount.incrementAndGet();
                                    }
                                    else
                                    {
                                        try
                                        {
                                            factory.passivateObject(underTest);
                                        }
                                        catch (Exception)
                                        {
                                            destroy(underTest);
                                            destroyedByEvictorCount.incrementAndGet();
                                        }
                                    }
                                }
                            }
                            if (!underTest.endEvictionTest(idleObjects))
                            {
                                // TODO - May need to add code here once additional
                                // states are used
                            }
                        }
                    }
                }
            }
            AbandonedConfig ac = this.abandonedConfig;
            if (ac != null && ac.RemoveAbandonedOnMaintenance)
            {
                removeAbandoned(ac);
            }
        }

        /// <summary>
        /// Attempts to create a new wrapped pooled object.
        /// <para>
        /// If there are <seealso cref="#getMaxTotal()"/> objects already in circulation
        /// or in process of being created, this method returns null.
        /// 
        /// </para>
        /// </summary>
        /// <returns> The new wrapped pooled object
        /// </returns>
        /// <exception cref="Exception"> if the object factory's {@code makeObject} fails </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private org.apache.commons.pool2.PooledObject<T> create() throws Exception
        private PooledObject<T> create()
        {
            int localMaxTotal = MaxTotal;
            long newCreateCount = createCount.incrementAndGet();
            if (localMaxTotal > -1 && newCreateCount > localMaxTotal || newCreateCount > int.MaxValue)
            {
                createCount.decrementAndGet();
                return null;
            }

//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final org.apache.commons.pool2.PooledObject<T> p;
            PooledObject<T> p;
            try
            {
                p = factory.makeObject();
            }
            catch (Exception e)
            {
                createCount.decrementAndGet();
                throw e;
            }

            AbandonedConfig ac = this.abandonedConfig;
            if (ac != null && ac.LogAbandoned)
            {
                p.LogAbandoned = true;
            }

            createdCount.incrementAndGet();
            allObjects.put(p.Object, p);
            return p;
        }

        /// <summary>
        /// Destroys a wrapped pooled object.
        /// </summary>
        /// <param name="toDestory"> The wrapped pooled object to destroy
        /// </param>
        /// <exception cref="Exception"> If the factory fails to destroy the pooled object
        ///                   cleanly </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private void destroy(org.apache.commons.pool2.PooledObject<T> toDestory) throws Exception
        private void destroy(PooledObject<T> toDestory)
        {
            toDestory.invalidate();
            idleObjects.remove(toDestory);
            allObjects.remove(toDestory.Object);
            try
            {
                factory.destroyObject(toDestory);
            }
            finally
            {
                destroyedCount.incrementAndGet();
                createCount.decrementAndGet();
            }
        }

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override void ensureMinIdle() throws Exception
        internal override void ensureMinIdle()
        {
            ensureIdle(MinIdle, true);
        }

        /// <summary>
        /// Tries to ensure that {@code idleCount} idle instances exist in the pool.
        /// <para>
        /// Creates and adds idle instances until either <seealso cref="#getNumIdle()"/> reaches {@code idleCount}
        /// or the total number of objects (idle, checked out, or being created) reaches
        /// <seealso cref="#getMaxTotal()"/>. If {@code always} is false, no instances are created unless
        /// there are threads waiting to check out instances from the pool.
        /// 
        /// </para>
        /// </summary>
        /// <param name="idleCount"> the number of idle instances desired </param>
        /// <param name="always"> true means create instances even if the pool has no threads waiting </param>
        /// <exception cref="Exception"> if the factory's makeObject throws </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private void ensureIdle(int idleCount, boolean always) throws Exception
        private void ensureIdle(int idleCount, bool always)
        {
            if (idleCount < 1 || Closed || (!always && !idleObjects.hasTakeWaiters()))
            {
                return;
            }

            while (idleObjects.size() < idleCount)
            {
                PooledObject<T> p = create();
                if (p == null)
                {
                    // Can't create objects, no reason to think another call to
                    // create will work. Give up.
                    break;
                }
                if (Lifo)
                {
                    idleObjects.addFirst(p);
                }
                else
                {
                    idleObjects.addLast(p);
                }
            }
        }

        /// <summary>
        /// Create an object, and place it into the pool. addObject() is useful for
        /// "pre-loading" a pool with idle objects.
        /// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void addObject() throws Exception
        public void addObject()
        {
            assertOpen();
            if (factory == null)
            {
                throw new java.lang.IllegalStateException("Cannot add objects without a factory.");
            }
            PooledObject<T> p = create();
            addIdleObject(p);
        }

        /// <summary>
        /// Add the provided wrapped pooled object to the set of idle objects for
        /// this pool. The object must already be part of the pool.
        /// </summary>
        /// <param name="p"> The object to make idle
        /// </param>
        /// <exception cref="Exception"> If the factory fails to passivate the object </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: private void addIdleObject(org.apache.commons.pool2.PooledObject<T> p) throws Exception
        private void addIdleObject(PooledObject<T> p)
        {
            if (p != null)
            {
                factory.passivateObject(p);
                if (Lifo)
                {
                    idleObjects.addFirst(p);
                }
                else
                {
                    idleObjects.addLast(p);
                }
            }
        }

        /// <summary>
        /// Calculate the number of objects to test in a run of the idle object
        /// evictor.
        /// </summary>
        /// <returns> The number of objects to test for validity </returns>
        private int NumTests
        {
            get
            {
                int numTestsPerEvictionRun = NumTestsPerEvictionRun;
                if (numTestsPerEvictionRun >= 0)
                {
                    return Math.Min(numTestsPerEvictionRun, idleObjects.size());
                }
                else
                {
                    return (int)(Math.Ceiling(idleObjects.size() / Math.Abs((double) numTestsPerEvictionRun)));
                }
            }
        }

        /// <summary>
        /// Recover abandoned objects which have been checked out but
        /// not used since longer than the removeAbandonedTimeout.
        /// </summary>
        /// <param name="ac"> The configuration to use to identify abandoned objects </param>
        private void removeAbandoned(AbandonedConfig ac)
        {
            // Generate a list of abandoned objects to remove
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final long now = System.currentTimeMillis();
            long now = java.lang.System.currentTimeMillis();
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final long timeout = now - (ac.getRemoveAbandonedTimeout() * 1000L);
            long timeout = now - (ac.RemoveAbandonedTimeout * 1000L);
            List remove = new LinkedList();
            Iterator it = allObjects.values().iterator();
            while (it.hasNext())
            {
                PooledObject<T> pooledObject = (PooledObject<T>)it.next();
                lock (pooledObject)
                {
                    if (pooledObject.State == PooledObjectState.ALLOCATED && pooledObject.LastUsedTime <= timeout)
                    {
                        pooledObject.markAbandoned();
                        remove.add(pooledObject);
                    }
                }
            }

            // Now remove the abandoned objects
            Iterator itr = remove.iterator();
            while (itr.hasNext())
            {
                PooledObject<T> pooledObject = (PooledObject<T>)itr.next();
                if (ac.LogAbandoned)
                {
                    pooledObject.printStackTrace(ac.LogWriter);
                }
                try
                {
                    invalidateObject(pooledObject.Object);
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.ToString());
                    Console.Write(e.StackTrace);
                }
            }
        }


        //--- Usage tracking support -----------------------------------------------

        public void use(T pooledObject)
        {
            AbandonedConfig ac = this.abandonedConfig;
            if (ac != null && ac.UseUsageTracking)
            {
                PooledObject<T> wrapper = (PooledObject<T>)allObjects.get(pooledObject);
                wrapper.use();
            }
        }


        //--- JMX support ----------------------------------------------------------

        private volatile string factoryType = null;

        /// <summary>
        /// Return an estimate of the number of threads currently blocked waiting for
        /// an object from the pool. This is intended for monitoring only, not for
        /// synchronization control.
        /// </summary>
        /// <returns> The estimate of the number of threads currently blocked waiting
        ///         for an object from the pool </returns>
        public int NumWaiters
        {
            get
            {
                if (BlockWhenExhausted)
                {
                    return idleObjects.TakeQueueLength;
                }
                else
                {
                    return 0;
                }
            }
        }

        /// <summary>
        /// Return the type - including the specific type rather than the generic -
        /// of the factory.
        /// </summary>
        /// <returns> A string representation of the factory type </returns>
        public string FactoryType
        {
            get
            {
                // Not thread safe. Accept that there may be multiple evaluations.
                if (factoryType == null)
                {
                    java.lang.StringBuilder result = new java.lang.StringBuilder();
    //JAVA TO C# CONVERTER WARNING: The .NET Type.FullName property will not always yield results identical to the Java Class.getName method:
                    result.append(factory.GetType().FullName);
                    result.append('<');
                    java.lang.Class pooledObjectType = PoolImplUtils.getFactoryType<T>((java.lang.Class)factory.GetType());
    //JAVA TO C# CONVERTER WARNING: The .NET Type.FullName property will not always yield results identical to the Java Class.getName method:
                    result.append(pooledObjectType.getName());
                    result.append('>');
                    factoryType = result.ToString();
                }
                return factoryType;
            }
        }

        /// <summary>
        /// Provides information on all the objects in the pool, both idle (waiting
        /// to be borrowed) and active (currently borrowed).
        /// <para>
        /// Note: This is named listAllObjects so it is presented as an operation via
        /// JMX. That means it won't be invoked unless the explicitly requested
        /// whereas all attributes will be automatically requested when viewing the
        /// attributes for an object in a tool like JConsole.
        /// 
        /// </para>
        /// </summary>
        /// <returns> Information grouped on all the objects in the pool </returns>
        public java.util.Set listAllObjects()
        {
            java.util.Set result = new java.util.HashSet(allObjects.size());
            Iterator it = allObjects.values().iterator();
            while(it.hasNext())
            {
                PooledObject<T> p = (PooledObject<T>)it.next();
                result.add(new DefaultPooledObjectInfo<T>(p));
            }
            return result;
        }

        // --- configuration attributes --------------------------------------------

        private volatile int maxIdle = GenericObjectPoolConfig.DEFAULT_MAX_IDLE;
        private volatile int minIdle = GenericObjectPoolConfig.DEFAULT_MIN_IDLE;
        private readonly PooledObjectFactory<T> factory;


        // --- internal attributes -------------------------------------------------

        /*
         * All of the objects currently associated with this pool in any state. It
         * excludes objects that have been destroyed. The size of
         * {@link #allObjects} will always be less than or equal to {@link
         * #_maxActive}. Map keys are pooled objects, values are the PooledObject
         * wrappers used internally by the pool.
         */
        private readonly Map allObjects = new java.util.concurrent.ConcurrentHashMap();
        /*
         * The combined count of the currently created objects and those in the
         * process of being created. Under load, it may exceed {@link #_maxActive}
         * if multiple threads try and create a new object at the same time but
         * {@link #create()} will ensure that there are never more than
         * {@link #_maxActive} objects created at any one time.
         */
        private readonly AtomicLong createCount = new AtomicLong(0);
        private readonly LinkedBlockingDeque<PooledObject<T>> idleObjects = new LinkedBlockingDeque<PooledObject<T>>();

        // JMX specific attributes
        private const string ONAME_BASE = "org.apache.commons.pool2:type=GenericObjectPool,name=";

        // Additional configuration properties for abandoned object tracking
        private volatile AbandonedConfig abandonedConfig = null;
    }

}