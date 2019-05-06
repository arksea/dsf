using java.util;
using java.util.concurrent.locks;

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
namespace org.apache.commons.pool2
{

    using AtomicLong = java.util.concurrent.atomic.AtomicLong;
	/// <summary>
	/// This class consists exclusively of static methods that operate on or return
	/// ObjectPool or KeyedObjectPool related interfaces.
	/// 
	/// @version $Revision: 1552588 $
	/// 
	/// @since 2.0
	/// </summary>
	public sealed class PoolUtils
	{

		/// <summary>
		/// Timer used to periodically check pools idle object count. Because a
		/// <seealso cref="Timer"/> creates a <seealso cref="Thread"/>, an IODH is used.
		/// </summary>
		internal class TimerHolder
		{
			internal static readonly java.util.Timer MIN_IDLE_TIMER = new java.util.Timer(true);
		}

		/// <summary>
		/// PoolUtils instances should NOT be constructed in standard programming.
		/// Instead, the class should be used procedurally: PoolUtils.adapt(aPool);.
		/// This constructor is public to permit tools that require a JavaBean
		/// instance to operate.
		/// </summary>
		public PoolUtils()
		{
		}

		/// <summary>
		/// Should the supplied Throwable be re-thrown (eg if it is an instance of
		/// one of the Throwables that should never be swallowed). Used by the pool
		/// error handling for operations that throw exceptions that normally need to
		/// be ignored.
		/// </summary>
		/// <param name="t">
		///            The Throwable to check </param>
		/// <exception cref="ThreadDeath">
		///             if that is passed in </exception>
		/// <exception cref="VirtualMachineError">
		///             if that is passed in </exception>
		public static void checkRethrow(System.Exception t)
		{
            if (t is java.lang.ThreadDeath)
			{
                throw (java.lang.ThreadDeath)t;
			}
            if (t is java.lang.VirtualMachineError)
			{
                throw (java.lang.VirtualMachineError)t;
			}
			// All other instances of Throwable will be silently swallowed
		}

		/// <summary>
		/// Periodically check the idle object count for the pool. At most one idle
		/// object will be added per period. If there is an exception when calling
		/// <seealso cref="ObjectPool#addObject()"/> then no more checks will be performed.
		/// </summary>
		/// <param name="pool">
		///            the pool to check periodically. </param>
		/// <param name="minIdle">
		///            if the <seealso cref="ObjectPool#getNumIdle()"/> is less than this then
		///            add an idle object. </param>
		/// <param name="period">
		///            the frequency to check the number of idle objects in a pool,
		///            see <seealso cref="Timer#schedule(TimerTask, long, long)"/>. </param>
		/// @param <T> the type of objects in the pool </param>
		/// <returns> the <seealso cref="TimerTask"/> that will periodically check the pools idle
		///         object count. </returns>
		/// <exception cref="IllegalArgumentException">
		///             when <code>pool</code> is <code>null</code> or when
		///             <code>minIdle</code> is negative or when <code>period</code>
		///             isn't valid for <seealso cref="Timer#schedule(TimerTask, long, long)"/> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public static <T> java.util.TimerTask checkMinIdle(final ObjectPool<T> pool, final int minIdle, final long period) throws IllegalArgumentException
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
		public static TimerTask checkMinIdle<T>(ObjectPool<T> pool, int minIdle, long period)
		{
			if (pool == null)
			{
				throw new System.ArgumentException("keyedPool must not be null.");
			}
			if (minIdle < 0)
			{
				throw new System.ArgumentException("minIdle must be non-negative.");
			}
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final java.util.TimerTask task = new ObjectPoolMinIdleTimerTask<T>(pool, minIdle);
			TimerTask task = new ObjectPoolMinIdleTimerTask<T>(pool, minIdle);
			MinIdleTimer.schedule(task, 0L, period);
			return task;
		}

		/// <summary>
		/// Call <code>addObject()</code> on <code>pool</code> <code>count</code>
		/// number of times.
		/// </summary>
		/// <param name="pool">
		///            the pool to prefill. </param>
		/// <param name="count">
		///            the number of idle objects to add. </param>
		/// @param <T> the type of objects in the pool </param>
		/// <exception cref="Exception">
		///             when <seealso cref="ObjectPool#addObject()"/> fails. </exception>
		/// <exception cref="IllegalArgumentException">
		///             when <code>pool</code> is <code>null</code>. </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public static <T> void prefill(final ObjectPool<T> pool, final int count) throws Exception, IllegalArgumentException
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
		public static void prefill<T>(ObjectPool<T> pool, int count)
		{
			if (pool == null)
			{
				throw new System.ArgumentException("pool must not be null.");
			}
			for (int i = 0; i < count; i++)
			{
				pool.addObject();
			}
		}

		/// <summary>
		/// Returns a synchronized (thread-safe) ObjectPool backed by the specified
		/// ObjectPool.
		/// <para>
		/// <b>Note:</b> This should not be used on pool implementations that already
		/// provide proper synchronization such as the pools provided in the Commons
		/// Pool library. Wrapping a pool that <seealso cref="#wait() waits"/> for poolable
		/// objects to be returned before allowing another one to be borrowed with
		/// another layer of synchronization will cause liveliness issues or a
		/// deadlock.
		/// </para>
		/// </summary>
		/// <param name="pool">
		///            the ObjectPool to be "wrapped" in a synchronized ObjectPool. </param>
		/// @param <T> the type of objects in the pool </param>
		/// <returns> a synchronized view of the specified ObjectPool. </returns>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: public static <T> ObjectPool<T> synchronizedPool(final ObjectPool<T> pool)
		public static ObjectPool<T> synchronizedPool<T>(ObjectPool<T> pool)
		{
			if (pool == null)
			{
				throw new System.ArgumentException("pool must not be null.");
			}
			/*
			 * assert !(pool instanceof GenericObjectPool) :
			 * "GenericObjectPool is already thread-safe"; assert !(pool instanceof
			 * SoftReferenceObjectPool) :
			 * "SoftReferenceObjectPool is already thread-safe"; assert !(pool
			 * instanceof StackObjectPool) :
			 * "StackObjectPool is already thread-safe"; assert
			 * !"org.apache.commons.pool.composite.CompositeObjectPool"
			 * .equals(pool.getClass().getName()) :
			 * "CompositeObjectPools are already thread-safe";
			 */
			return new SynchronizedObjectPool<T>(pool);
		}

		/// <summary>
		/// Returns a synchronized (thread-safe) PooledObjectFactory backed by the
		/// specified PooledObjectFactory.
		/// </summary>
		/// <param name="factory">
		///            the PooledObjectFactory to be "wrapped" in a synchronized
		///            PooledObjectFactory. </param>
		/// @param <T> the type of objects in the pool </param>
		/// <returns> a synchronized view of the specified PooledObjectFactory. </returns>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: public static <T> PooledObjectFactory<T> synchronizedPooledFactory(final PooledObjectFactory<T> factory)
		public static PooledObjectFactory<T> synchronizedPooledFactory<T>(PooledObjectFactory<T> factory)
		{
			return new SynchronizedPooledObjectFactory<T>(factory);
		}

		/// <summary>
		/// Returns a pool that adaptively decreases its size when idle objects are
		/// no longer needed. This is intended as an always thread-safe alternative
		/// to using an idle object evictor provided by many pool implementations.
		/// This is also an effective way to shrink FIFO ordered pools that
		/// experience load spikes.
		/// </summary>
		/// <param name="pool">
		///            the ObjectPool to be decorated so it shrinks its idle count
		///            when possible. </param>
		/// @param <T> the type of objects in the pool </param>
		/// <returns> a pool that adaptively decreases its size when idle objects are
		///         no longer needed. </returns>
		/// <seealso cref= #erodingPool(ObjectPool, float) </seealso>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: public static <T> ObjectPool<T> erodingPool(final ObjectPool<T> pool)
		public static ObjectPool<T> erodingPool<T>(ObjectPool<T> pool)
		{
			return erodingPool(pool, 1f);
		}

		/// <summary>
		/// Returns a pool that adaptively decreases its size when idle objects are
		/// no longer needed. This is intended as an always thread-safe alternative
		/// to using an idle object evictor provided by many pool implementations.
		/// This is also an effective way to shrink FIFO ordered pools that
		/// experience load spikes.
		/// <para>
		/// The factor parameter provides a mechanism to tweak the rate at which the
		/// pool tries to shrink its size. Values between 0 and 1 cause the pool to
		/// try to shrink its size more often. Values greater than 1 cause the pool
		/// to less frequently try to shrink its size.
		/// </para>
		/// </summary>
		/// <param name="pool">
		///            the ObjectPool to be decorated so it shrinks its idle count
		///            when possible. </param>
		/// <param name="factor">
		///            a positive value to scale the rate at which the pool tries to
		///            reduce its size. If 0 &lt; factor &lt; 1 then the pool
		///            shrinks more aggressively. If 1 &lt; factor then the pool
		///            shrinks less aggressively. </param>
		/// @param <T> the type of objects in the pool </param>
		/// <returns> a pool that adaptively decreases its size when idle objects are
		///         no longer needed. </returns>
		/// <seealso cref= #erodingPool(ObjectPool) </seealso>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: public static <T> ObjectPool<T> erodingPool(final ObjectPool<T> pool, final float factor)
		public static ObjectPool<T> erodingPool<T>(ObjectPool<T> pool, float factor)
		{
			if (pool == null)
			{
				throw new System.ArgumentException("pool must not be null.");
			}
			if (factor <= 0f)
			{
				throw new System.ArgumentException("factor must be positive.");
			}
			return new ErodingObjectPool<T>(pool, factor);
		}

		/// <summary>
		/// Get the <code>Timer</code> for checking keyedPool's idle count.
		/// </summary>
		/// <returns> the <seealso cref="Timer"/> for checking keyedPool's idle count. </returns>
		private static Timer MinIdleTimer
		{
			get
			{
				return TimerHolder.MIN_IDLE_TIMER;
			}
		}

		/// <summary>
		/// Timer task that adds objects to the pool until the number of idle
		/// instances reaches the configured minIdle. Note that this is not the same
		/// as the pool's minIdle setting.
		/// </summary>
		private class ObjectPoolMinIdleTimerTask<T> : TimerTask
		{

			/// <summary>
			/// Minimum number of idle instances. Not the same as pool.getMinIdle(). </summary>
			internal readonly int minIdle;

			/// <summary>
			/// Object pool </summary>
			internal readonly ObjectPool<T> pool;

			/// <summary>
			/// Create a new ObjectPoolMinIdleTimerTask for the given pool with the
			/// given minIdle setting.
			/// </summary>
			/// <param name="pool">
			///            object pool </param>
			/// <param name="minIdle">
			///            number of idle instances to maintain </param>
			/// <exception cref="IllegalArgumentException">
			///             if the pool is null </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: ObjectPoolMinIdleTimerTask(final ObjectPool<T> pool, final int minIdle) throws IllegalArgumentException
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
			internal ObjectPoolMinIdleTimerTask(ObjectPool<T> pool, int minIdle)
			{
				if (pool == null)
				{
					throw new System.ArgumentException("pool must not be null.");
				}
				this.pool = pool;
				this.minIdle = minIdle;
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public override void run()
			{
				bool success = false;
				try
				{
					if (pool.NumIdle < minIdle)
					{
						pool.addObject();
					}
					success = true;

				}
				catch (System.Exception)
				{
					cancel();
				}
				finally
				{
					// detect other types of Throwable and cancel this Timer
					if (!success)
					{
						cancel();
					}
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public new string ToString()
			{
                java.lang.StringBuilder sb = new java.lang.StringBuilder();
                sb.append("ObjectPoolMinIdleTimerTask");
                sb.append("{minIdle=").append(minIdle);
                sb.append(", pool=").append(pool);
                sb.append('}');
                return sb.toString();
			}
		}

		/// <summary>
		/// A synchronized (thread-safe) ObjectPool backed by the specified
		/// ObjectPool.
		/// <para>
		/// <b>Note:</b> This should not be used on pool implementations that already
		/// provide proper synchronization such as the pools provided in the Commons
		/// Pool library. Wrapping a pool that <seealso cref="#wait() waits"/> for poolable
		/// objects to be returned before allowing another one to be borrowed with
		/// another layer of synchronization will cause liveliness issues or a
		/// deadlock.
		/// </para>
		/// </summary>
		private class SynchronizedObjectPool<T> : ObjectPool<T>
		{

			/// <summary>
			/// Object whose monitor is used to synchronize methods on the wrapped
			/// pool.
			/// </summary>
			internal readonly ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();

			/// <summary>
			/// the underlying object pool </summary>
			internal readonly ObjectPool<T> pool;

			/// <summary>
			/// Create a new SynchronizedObjectPool wrapping the given pool.
			/// </summary>
			/// <param name="pool">
			///            the ObjectPool to be "wrapped" in a synchronized
			///            ObjectPool. </param>
			/// <exception cref="IllegalArgumentException">
			///             if the pool is null </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: SynchronizedObjectPool(final ObjectPool<T> pool) throws IllegalArgumentException
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
			internal SynchronizedObjectPool(ObjectPool<T> pool)
			{
				if (pool == null)
				{
					throw new System.ArgumentException("pool must not be null.");
				}
				this.pool = pool;
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public T borrowObject() throws Exception, java.util.NoSuchElementException, IllegalStateException
			public T borrowObject()
			{
				ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
				writeLock.@lock();
				try
				{
					return pool.borrowObject();
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: @Override public void returnObject(final T obj)
			public void returnObject(T obj)
			{
				ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
				writeLock.@lock();
				try
				{
					pool.returnObject(obj);
				}
				catch (System.Exception)
				{
					// swallowed as of Pool 2
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: @Override public void invalidateObject(final T obj)
			public void invalidateObject(T obj)
			{
				ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
				writeLock.@lock();
				try
				{
					pool.invalidateObject(obj);
				}
                catch (System.Exception)
				{
					// swallowed as of Pool 2
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void addObject() throws Exception, IllegalStateException, UnsupportedOperationException
			public void addObject()
			{
				ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
				writeLock.@lock();
				try
				{
					pool.addObject();
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public int NumIdle
			{
				get
				{
					ReentrantReadWriteLock.ReadLock readLock = readWriteLock.readLock();
					readLock.@lock();
					try
					{
						return pool.NumIdle;
					}
					finally
					{
						readLock.unlock();
					}
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public int NumActive
			{
				get
				{
					ReentrantReadWriteLock.ReadLock readLock = readWriteLock.readLock();
					readLock.@lock();
					try
					{
						return pool.NumActive;
					}
					finally
					{
						readLock.unlock();
					}
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void clear() throws Exception, UnsupportedOperationException
			public void clear()
			{
				ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
				writeLock.@lock();
				try
				{
					pool.clear();
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public void close()
			{
				ReentrantReadWriteLock.WriteLock writeLock = readWriteLock.writeLock();
				writeLock.@lock();
				try
				{
					pool.close();
				}
                catch (System.Exception)
				{
					// swallowed as of Pool 2
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public override string ToString()
			{
                java.lang.StringBuilder sb = new java.lang.StringBuilder();
                sb.append("SynchronizedObjectPool");
                sb.append("{pool=").append(pool);
                sb.append('}');
                return sb.toString();
			}
		}

		/// <summary>
		/// A fully synchronized PooledObjectFactory that wraps a
		/// PooledObjectFactory and synchronizes access to the wrapped factory
		/// methods.
		/// <para>
		/// <b>Note:</b> This should not be used on pool implementations that already
		/// provide proper synchronization such as the pools provided in the Commons
		/// Pool library.
		/// </para>
		/// </summary>
		private class SynchronizedPooledObjectFactory<T> : PooledObjectFactory<T>
		{
			/// <summary>
			/// Synchronization lock </summary>
			internal readonly ReentrantReadWriteLock.WriteLock writeLock = new ReentrantReadWriteLock().writeLock();

			/// <summary>
			/// Wrapped factory </summary>
			internal readonly PooledObjectFactory<T> factory;

			/// <summary>
			/// Create a SynchronizedPoolableObjectFactory wrapping the given
			/// factory.
			/// </summary>
			/// <param name="factory">
			///            underlying factory to wrap </param>
			/// <exception cref="IllegalArgumentException">
			///             if the factory is null </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: SynchronizedPooledObjectFactory(final PooledObjectFactory<T> factory) throws IllegalArgumentException
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
			internal SynchronizedPooledObjectFactory(PooledObjectFactory<T> factory)
			{
				if (factory == null)
				{
					throw new System.ArgumentException("factory must not be null.");
				}
				this.factory = factory;
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public PooledObject<T> makeObject() throws Exception
			public PooledObject<T> makeObject()
			{
				writeLock.@lock();
				try
				{
					return factory.makeObject();
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void destroyObject(final PooledObject<T> p) throws Exception
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
			public void destroyObject(PooledObject<T> p)
			{
				writeLock.@lock();
				try
				{
					factory.destroyObject(p);
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: @Override public boolean validateObject(final PooledObject<T> p)
			public bool validateObject(PooledObject<T> p)
			{
				writeLock.@lock();
				try
				{
					return factory.validateObject(p);
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void activateObject(final PooledObject<T> p) throws Exception
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
			public void activateObject(PooledObject<T> p)
			{
				writeLock.@lock();
				try
				{
					factory.activateObject(p);
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void passivateObject(final PooledObject<T> p) throws Exception
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
			public void passivateObject(PooledObject<T> p)
			{
				writeLock.@lock();
				try
				{
					factory.passivateObject(p);
				}
				finally
				{
					writeLock.unlock();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public override string ToString()
			{
                java.lang.StringBuilder sb = new java.lang.StringBuilder();
                sb.append("SynchronizedPoolableObjectFactory");
                sb.append("{factory=").append(factory);
                sb.append('}');
                return sb.toString();
			}
		}

		/// <summary>
		/// Encapsulate the logic for when the next poolable object should be
		/// discarded. Each time update is called, the next time to shrink is
		/// recomputed, based on the float factor, number of idle instances in the
		/// pool and high water mark. Float factor is assumed to be between 0 and 1.
		/// Values closer to 1 cause less frequent erosion events. Erosion event
		/// timing also depends on numIdle. When this value is relatively high (close
		/// to previously established high water mark), erosion occurs more
		/// frequently.
		/// </summary>
		private class ErodingFactor
		{
			/// <summary>
			/// Determines frequency of "erosion" events </summary>
			internal readonly float factor;

			/// <summary>
			/// Time of next shrink event </summary>
			internal AtomicLong nextShrink = new AtomicLong(0);

			/// <summary>
			/// High water mark - largest numIdle encountered </summary>
			internal volatile int idleHighWaterMark;

			/// <summary>
			/// Create a new ErodingFactor with the given erosion factor.
			/// </summary>
			/// <param name="factor">
			///            erosion factor </param>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: public ErodingFactor(final float factor)
			public ErodingFactor(float factor)
			{
                this.factor = factor;
                nextShrink.set(java.lang.System.currentTimeMillis() + (long)(900000 * factor)); // now
                // +
                // 15
                // min
                // *
                // factor
                idleHighWaterMark = 1;
			}

			/// <summary>
			/// Updates internal state using the supplied time and numIdle.
			/// </summary>
			/// <param name="now">
			///            current time </param>
			/// <param name="numIdle">
			///            number of idle elements in the pool </param>
			public virtual void update(long now, int numIdle)
			{
				int idle = java.lang.Math.max(0, numIdle);
				idleHighWaterMark = java.lang.Math.max(idle, idleHighWaterMark);
				const float maxInterval = 15f;
				float minutes = maxInterval + ((1f - maxInterval) / idleHighWaterMark) * idle;
				nextShrink.set(now + (long)(minutes * 60000f * factor));
			}

			/// <summary>
			/// Returns the time of the next erosion event.
			/// </summary>
			/// <returns> next shrink time </returns>
			public virtual long NextShrink
			{
				get
				{
					return nextShrink.get();
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public override string ToString()
			{
				return "ErodingFactor{" + "factor=" + factor + ", idleHighWaterMark=" + idleHighWaterMark + '}';
			}
		}

		/// <summary>
		/// Decorates an object pool, adding "eroding" behavior. Based on the
		/// configured <seealso cref="#factor erosion factor"/>, objects returning to the pool
		/// may be invalidated instead of being added to idle capacity.
		/// </summary>
		private class ErodingObjectPool<T> : ObjectPool<T>
		{
			/// <summary>
			/// Underlying object pool </summary>
			internal readonly ObjectPool<T> pool;

			/// <summary>
			/// Erosion factor </summary>
			internal readonly ErodingFactor factor;

			/// <summary>
			/// Create an ErodingObjectPool wrapping the given pool using the
			/// specified erosion factor.
			/// </summary>
			/// <param name="pool">
			///            underlying pool </param>
			/// <param name="factor">
			///            erosion factor - determines the frequency of erosion
			///            events </param>
			/// <seealso cref= #factor </seealso>
			public ErodingObjectPool(ObjectPool<T> pool, float factor)
			{
				this.pool = pool;
				this.factor = new ErodingFactor(factor);
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public T borrowObject()
			{
				return pool.borrowObject();
			}

			/// <summary>
			/// Returns obj to the pool, unless erosion is triggered, in which case
			/// obj is invalidated. Erosion is triggered when there are idle
			/// instances in the pool and more than the {@link #factor erosion
			/// factor}-determined time has elapsed since the last returnObject
			/// activation.
			/// </summary>
			/// <param name="obj">
			///            object to return or invalidate </param>
			/// <seealso cref= #factor </seealso>
			public void returnObject(T obj)
			{
				bool discard = false;
                long now = java.lang.System.currentTimeMillis();
				lock (pool)
				{
					if (factor.NextShrink < now) // XXX: Pool 3: move test
					{
														// out of sync block
						int numIdle = pool.NumIdle;
						if (numIdle > 0)
						{
							discard = true;
						}

						factor.update(now, numIdle);
					}
				}
				try
				{
					if (discard)
					{
						pool.invalidateObject(obj);
					}
					else
					{
						pool.returnObject(obj);
					}
				}
				catch (System.Exception)
				{
					// swallowed
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
//JAVA TO C# CONVERTER WARNING: 'final' parameters are not available in .NET:
//ORIGINAL LINE: @Override public void invalidateObject(final T obj)
			public void invalidateObject(T obj)
			{
				try
				{
					pool.invalidateObject(obj);
				}
				catch (System.Exception)
				{
					// swallowed
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public void addObject()
			{
				pool.addObject();
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public int NumIdle
			{
				get
				{
					return pool.NumIdle;
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public int NumActive
			{
				get
				{
					return pool.NumActive;
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public void clear()
			{
				pool.clear();
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public void close()
			{
				try
				{
					pool.close();
				}
				catch (System.Exception)
				{
					// swallowed
				}
			}

			/// <summary>
			/// {@inheritDoc}
			/// </summary>
			public override string ToString()
			{
				return "ErodingObjectPool{" + "factor=" + factor + ", pool=" + pool + '}';
			}
		}

	}
}