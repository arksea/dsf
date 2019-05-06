using System;

namespace com.google.common.util.concurrent
{

	using com.google.common.@base;

	//using Beta = com.google.common.annotations.Beta;
	//using VisibleForTesting = com.google.common.annotations.VisibleForTesting;
	using Stopwatch = com.google.common.@base.Stopwatch;


	/// <summary>
	/// A rate limiter. Conceptually, a rate limiter distributes permits at a
	/// configurable rate. Each <seealso cref="#acquire()"/> blocks if necessary until a permit is
	/// available, and then takes it. Once acquired, permits need not be released.
	/// 
	/// <para>Rate limiters are often used to restrict the rate at which some
	/// physical or logical resource is accessed. This is in contrast to {@link
	/// java.util.concurrent.Semaphore} which restricts the number of concurrent
	/// accesses instead of the rate (note though that concurrency and rate are closely related,
	/// e.g. see <a href="http://en.wikipedia.org/wiki/Little's_law">Little's Law</a>).
	/// 
	/// </para>
	/// <para>A {@code RateLimiter} is defined primarily by the rate at which permits
	/// are issued. Absent additional configuration, permits will be distributed at a
	/// fixed rate, defined in terms of permits per second. Permits will be distributed
	/// smoothly, with the delay between individual permits being adjusted to ensure
	/// that the configured rate is maintained.
	/// 
	/// </para>
	/// <para>It is possible to configure a {@code RateLimiter} to have a warmup
	/// period during which time the permits issued each second steadily increases until
	/// it hits the stable rate.
	/// 
	/// </para>
	/// <para>As an example, imagine that we have a list of tasks to execute, but we don't want to
	/// submit more than 2 per second:
	/// <pre>  {@code
	///  final RateLimiter rateLimiter = RateLimiter.create(2.0); // rate is "2 permits per second"
	///  void submitTasks(List<Runnable> tasks, Executor executor) {
	///    for (Runnable task : tasks) {
	///      rateLimiter.acquire(); // may wait
	///      executor.execute(task);
	///    }
	///  }
	/// }</pre>
	/// 
	/// </para>
	/// <para>As another example, imagine that we produce a stream of data, and we want to cap it
	/// at 5kb per second. This could be accomplished by requiring a permit per byte, and specifying
	/// a rate of 5000 permits per second:
	/// <pre>  {@code
	///  final RateLimiter rateLimiter = RateLimiter.create(5000.0); // rate = 5000 permits per second
	///  void submitPacket(byte[] packet) {
	///    rateLimiter.acquire(packet.length);
	///    networkService.send(packet);
	///  }
	/// }</pre>
	/// 
	/// </para>
	/// <para>It is important to note that the number of permits requested <i>never</i>
	/// affect the throttling of the request itself (an invocation to {@code acquire(1)}
	/// and an invocation to {@code acquire(1000)} will result in exactly the same throttling, if any),
	/// but it affects the throttling of the <i>next</i> request. I.e., if an expensive task
	/// arrives at an idle RateLimiter, it will be granted immediately, but it is the <i>next</i>
	/// request that will experience extra throttling, thus paying for the cost of the expensive
	/// task.
	/// 
	/// </para>
	/// <para>Note: {@code RateLimiter} does not provide fairness guarantees.
	/// 
	/// @author Dimitris Andreou
	/// @since 13.0
	/// </para>
	/// </summary>
	// TODO(user): switch to nano precision. A natural unit of cost is "bytes", and a micro precision
	//     would mean a maximum rate of "1MB/s", which might be small in some cases.
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @ThreadSafe @Beta public abstract class RateLimiter
	public abstract class RateLimiter
	{
	  /// <summary>
	  /// Creates a {@code RateLimiter} with the specified stable throughput, given as
	  /// "permits per second" (commonly referred to as <i>QPS</i>, queries per second).
	  /// 
	  /// <para>The returned {@code RateLimiter} ensures that on average no more than {@code
	  /// permitsPerSecond} are issued during any given second, with sustained requests
	  /// being smoothly spread over each second. When the incoming request rate exceeds
	  /// {@code permitsPerSecond} the rate limiter will release one permit every {@code
	  /// (1.0 / permitsPerSecond)} seconds. When the rate limiter is unused,
	  /// bursts of up to {@code permitsPerSecond} permits will be allowed, with subsequent
	  /// requests being smoothly limited at the stable rate of {@code permitsPerSecond}.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <param name="permitsPerSecond"> the rate of the returned {@code RateLimiter}, measured in
	  ///        how many permits become available per second </param>
	  /// <exception cref="IllegalArgumentException"> if {@code permitsPerSecond} is negative or zero </exception>
	  // TODO(user): "This is equivalent to
	  //                 {@code createWithCapacity(permitsPerSecond, 1, TimeUnit.SECONDS)}".
	  public static RateLimiter create(double permitsPerSecond)
	  {
		/*
		 * The default RateLimiter configuration can save the unused permits of up to one second.
		 * This is to avoid unnecessary stalls in situations like this: A RateLimiter of 1qps,
		 * and 4 threads, all calling acquire() at these moments:
		 *
		 * T0 at 0 seconds
		 * T1 at 1.05 seconds
		 * T2 at 2 seconds
		 * T3 at 3 seconds
		 *
		 * Due to the slight delay of T1, T2 would have to sleep till 2.05 seconds,
		 * and T3 would also have to sleep till 3.05 seconds.
		 */
		return create(SleepingStopwatch.createFromSystemTimer(), permitsPerSecond);
	  }

	  /*
	   * TODO(cpovirk): make SleepingStopwatch the last parameter throughout the class so that the
	   * overloads follow the usual convention: Foo(int), Foo(int, SleepingStopwatch)
	   */
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @VisibleForTesting static RateLimiter create(SleepingStopwatch stopwatch, double permitsPerSecond)
	  internal static RateLimiter create(SleepingStopwatch stopwatch, double permitsPerSecond)
	  {
		RateLimiter rateLimiter = new SmoothRateLimiter.SmoothBursty(stopwatch, 1.0); // maxBurstSeconds
		rateLimiter.Rate = permitsPerSecond;
		return rateLimiter;
	  }

	  /// <summary>
	  /// Creates a {@code RateLimiter} with the specified stable throughput, given as
	  /// "permits per second" (commonly referred to as <i>QPS</i>, queries per second), and a
	  /// <i>warmup period</i>, during which the {@code RateLimiter} smoothly ramps up its rate,
	  /// until it reaches its maximum rate at the end of the period (as long as there are enough
	  /// requests to saturate it). Similarly, if the {@code RateLimiter} is left <i>unused</i> for
	  /// a duration of {@code warmupPeriod}, it will gradually return to its "cold" state,
	  /// i.e. it will go through the same warming up process as when it was first created.
	  /// 
	  /// <para>The returned {@code RateLimiter} is intended for cases where the resource that actually
	  /// fulfills the requests (e.g., a remote server) needs "warmup" time, rather than
	  /// being immediately accessed at the stable (maximum) rate.
	  /// 
	  /// </para>
	  /// <para>The returned {@code RateLimiter} starts in a "cold" state (i.e. the warmup period
	  /// will follow), and if it is left unused for long enough, it will return to that state.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <param name="permitsPerSecond"> the rate of the returned {@code RateLimiter}, measured in
	  ///        how many permits become available per second </param>
	  /// <param name="warmupPeriod"> the duration of the period where the {@code RateLimiter} ramps up its
	  ///        rate, before reaching its stable (maximum) rate </param>
	  /// <param name="unit"> the time unit of the warmupPeriod argument </param>
	  /// <exception cref="IllegalArgumentException"> if {@code permitsPerSecond} is negative or zero or
	  ///     {@code warmupPeriod} is negative </exception>
	  public static RateLimiter create(double permitsPerSecond, long warmupPeriodMicros)
	  {
		Preconditions.checkArgument(warmupPeriodMicros >= 0, "warmupPeriod must not be negative: %s", warmupPeriodMicros);
		return create(SleepingStopwatch.createFromSystemTimer(), permitsPerSecond, warmupPeriodMicros, 3.0);
	  }

//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @VisibleForTesting static RateLimiter create(SleepingStopwatch stopwatch, double permitsPerSecond, long warmupPeriod, java.util.concurrent.TimeUnit unit, double coldFactor)
	  internal static RateLimiter create(SleepingStopwatch stopwatch, double permitsPerSecond, long warmupPeriodMicros, double coldFactor)
	  {
		RateLimiter rateLimiter = new SmoothRateLimiter.SmoothWarmingUp(stopwatch, warmupPeriodMicros, coldFactor);
		rateLimiter.Rate = permitsPerSecond;
		return rateLimiter;
	  }

	  /// <summary>
	  /// The underlying timer; used both to measure elapsed time and sleep as necessary. A separate
	  /// object to facilitate testing.
	  /// </summary>
	  private readonly SleepingStopwatch stopwatch;

	  // Can't be initialized in the constructor because mocks don't call the constructor.
	  private volatile object mutexDoNotUseDirectly;

	  private object mutex()
	  {
		object mutex = mutexDoNotUseDirectly;
		if (mutex == null)
		{
		  lock (this)
		  {
			mutex = mutexDoNotUseDirectly;
			if (mutex == null)
			{
			  mutexDoNotUseDirectly = mutex = new object();
			}
		  }
		}
		return mutex;
	  }

	  internal RateLimiter(SleepingStopwatch stopwatch)
	  {
          if (stopwatch == null) {
              throw new ArgumentNullException("msg", "the msg can't be null");
          }
		this.stopwatch = Preconditions.checkNotNull(stopwatch);
	  }

	  /// <summary>
	  /// Updates the stable rate of this {@code RateLimiter}, that is, the
	  /// {@code permitsPerSecond} argument provided in the factory method that
	  /// constructed the {@code RateLimiter}. Currently throttled threads will <b>not</b>
	  /// be awakened as a result of this invocation, thus they do not observe the new rate;
	  /// only subsequent requests will.
	  /// 
	  /// <para>Note though that, since each request repays (by waiting, if necessary) the cost
	  /// of the <i>previous</i> request, this means that the very next request
	  /// after an invocation to {@code setRate} will not be affected by the new rate;
	  /// it will pay the cost of the previous request, which is in terms of the previous rate.
	  /// 
	  /// </para>
	  /// <para>The behavior of the {@code RateLimiter} is not modified in any other way,
	  /// e.g. if the {@code RateLimiter} was configured with a warmup period of 20 seconds,
	  /// it still has a warmup period of 20 seconds after this method invocation.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <param name="permitsPerSecond"> the new stable rate of this {@code RateLimiter} </param>
	  /// <exception cref="IllegalArgumentException"> if {@code permitsPerSecond} is negative or zero </exception>
	  public double Rate
	  {
		  set
		  {
			Preconditions.checkArgument(value > 0.0 && !double.IsNaN(value), "rate must be positive");
			lock (mutex())
			{
			  doSetRate(value, stopwatch.readMicros());
			}
		  }
		  get
		  {
			lock (mutex())
			{
			  return doGetRate();
			}
		  }
	  }

	  internal abstract void doSetRate(double permitsPerSecond, long nowMicros);


	  internal abstract double doGetRate();

	  /// <summary>
	  /// Acquires a single permit from this {@code RateLimiter}, blocking until the
	  /// request can be granted. Tells the amount of time slept, if any.
	  /// 
	  /// <para>This method is equivalent to {@code acquire(1)}.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <returns> time spent sleeping to enforce rate, in seconds; 0.0 if not rate-limited
	  /// @since 16.0 (present in 13.0 with {@code void} return type}) </returns>
	  public virtual double acquire()
	  {
		return acquire(1);
	  }

	  /// <summary>
	  /// Acquires the given number of permits from this {@code RateLimiter}, blocking until the
	  /// request can be granted. Tells the amount of time slept, if any.
	  /// </summary>
	  /// <param name="permits"> the number of permits to acquire </param>
	  /// <returns> time spent sleeping to enforce rate, in seconds; 0.0 if not rate-limited </returns>
	  /// <exception cref="IllegalArgumentException"> if the requested number of permits is negative or zero
	  /// @since 16.0 (present in 13.0 with {@code void} return type}) </exception>
	  public virtual double acquire(int permits)
	  {
		long microsToWait = reserve(permits);
		stopwatch.sleepMicrosUninterruptibly(microsToWait);
		return 1.0 * microsToWait / 1000000;
	  }

	  /// <summary>
	  /// Reserves the given number of permits from this {@code RateLimiter} for future use, returning
	  /// the number of microseconds until the reservation can be consumed.
	  /// </summary>
	  /// <returns> time in microseconds to wait until the resource can be acquired, never negative </returns>
	  internal long reserve(int permits)
	  {
		checkPermits(permits);
		lock (mutex())
		{
		  return reserveAndGetWaitLength(permits, stopwatch.readMicros());
		}
	  }

	  /// <summary>
	  /// Acquires a permit from this {@code RateLimiter} if it can be obtained
	  /// without exceeding the specified {@code timeout}, or returns {@code false}
	  /// immediately (without waiting) if the permit would not have been granted
	  /// before the timeout expired.
	  /// 
	  /// <para>This method is equivalent to {@code tryAcquire(1, timeout, unit)}.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <param name="timeout"> the maximum time to wait for the permit. Negative values are treated as zero. </param>
	  /// <param name="unit"> the time unit of the timeout argument </param>
	  /// <returns> {@code true} if the permit was acquired, {@code false} otherwise </returns>
	  /// <exception cref="IllegalArgumentException"> if the requested number of permits is negative or zero </exception>
	  public virtual bool tryAcquire(long timeoutMicros)
	  {
		return tryAcquire(1, timeoutMicros);
	  }

	  /// <summary>
	  /// Acquires permits from this <seealso cref="RateLimiter"/> if it can be acquired immediately without delay.
	  /// 
	  /// <para>
	  /// This method is equivalent to {@code tryAcquire(permits, 0, anyUnit)}.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <param name="permits"> the number of permits to acquire </param>
	  /// <returns> {@code true} if the permits were acquired, {@code false} otherwise </returns>
	  /// <exception cref="IllegalArgumentException"> if the requested number of permits is negative or zero
	  /// @since 14.0 </exception>
	  public virtual bool tryAcquire(int permits)
	  {
		    return tryAcquire(permits, 0);
	  }

	  /// <summary>
	  /// Acquires a permit from this <seealso cref="RateLimiter"/> if it can be acquired immediately without
	  /// delay.
	  /// 
	  /// <para>
	  /// This method is equivalent to {@code tryAcquire(1)}.
	  /// 
	  /// </para>
	  /// </summary>
	  /// <returns> {@code true} if the permit was acquired, {@code false} otherwise
	  /// @since 14.0 </returns>
	  public virtual bool tryAcquire()
	  {
		    return tryAcquire(1, 0);
	  }

	  /// <summary>
	  /// Acquires the given number of permits from this {@code RateLimiter} if it can be obtained
	  /// without exceeding the specified {@code timeout}, or returns {@code false}
	  /// immediately (without waiting) if the permits would not have been granted
	  /// before the timeout expired.
	  /// </summary>
	  /// <param name="permits"> the number of permits to acquire </param>
	  /// <param name="timeout"> the maximum time to wait for the permits. Negative values are treated as zero. </param>
	  /// <param name="unit"> the time unit of the timeout argument </param>
	  /// <returns> {@code true} if the permits were acquired, {@code false} otherwise </returns>
	  /// <exception cref="IllegalArgumentException"> if the requested number of permits is negative or zero </exception>
	  public virtual bool tryAcquire(int permits, long micros)
	  {
		long timeoutMicros = Math.Max(micros, 0);
		checkPermits(permits);
		long microsToWait;
		lock (mutex())
		{
		  long nowMicros = stopwatch.readMicros();
		  if (!canAcquire(nowMicros, timeoutMicros))
		  {
			return false;
		  }
		  else
		  {
			microsToWait = reserveAndGetWaitLength(permits, nowMicros);
		  }
		}
		stopwatch.sleepMicrosUninterruptibly(microsToWait);
		return true;
	  }

	  private bool canAcquire(long nowMicros, long timeoutMicros)
	  {
		return queryEarliestAvailable(nowMicros) - timeoutMicros <= nowMicros;
	  }

	  /// <summary>
	  /// Reserves next ticket and returns the wait time that the caller must wait for.
	  /// </summary>
	  /// <returns> the required wait time, never negative </returns>
	  internal long reserveAndGetWaitLength(int permits, long nowMicros)
	  {
		long momentAvailable = reserveEarliestAvailable(permits, nowMicros);
		return Math.Max(momentAvailable - nowMicros, 0);
	  }

	  /// <summary>
	  /// Returns the earliest time that permits are available (with one caveat).
	  /// </summary>
	  /// <returns> the time that permits are available, or, if permits are available immediately, an
	  ///     arbitrary past or present time </returns>
	  internal abstract long queryEarliestAvailable(long nowMicros);

		/// <summary>
		/// Reserves the requested number of permits and returns the time that those permits can be used
		/// (with one caveat).
		/// </summary>
		/// <returns> the time that the permits may be used, or, if the permits may be used immediately, an
		///     arbitrary past or present time </returns>
	  internal abstract long reserveEarliestAvailable(int permits, long nowMicros);

	  public override string ToString()
	  {
		return string.Format("RateLimiter[stableRate={0,3:F1}qps]", Rate);
	  }

//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @VisibleForTesting abstract static class SleepingStopwatch
	  internal abstract class SleepingStopwatch
	  {
		/*
		 * We always hold the mutex when calling this. TODO(cpovirk): Is that important? Perhaps we need
		 * to guarantee that each call to reserveEarliestAvailable, etc. sees a value >= the previous?
		 * Also, is it OK that we don't hold the mutex when sleeping?
		 */
		internal abstract long readMicros();

		internal abstract void sleepMicrosUninterruptibly(long micros);

		internal static SleepingStopwatch createFromSystemTimer()
		{
		  return new SleepingStopwatchAnonymousInnerClassHelper();
		}

		private class SleepingStopwatchAnonymousInnerClassHelper : SleepingStopwatch
		{
			public SleepingStopwatchAnonymousInnerClassHelper()
			{
			}

			internal readonly Stopwatch stopwatch = Stopwatch.createStarted();

			internal override long readMicros()
			{
                return stopwatch.elapsedMicros();
			}

			internal override void sleepMicrosUninterruptibly(long micros)
			{
			  if (micros > 0)
			  {
                  System.Threading.Thread.Sleep((int)micros);
			  }
			}
		}
	  }

	  private static int checkPermits(int permits)
	  {
		Preconditions.checkArgument(permits > 0, "Requested permits (%s) must be positive", permits);
		return permits;
	  }
	}

}