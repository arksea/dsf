using System;

/*
 * Copyright (C) 2008 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace com.google.common.@base
{
	/// <summary>
	/// An object that measures elapsed time in nanoseconds. It is useful to measure
	/// elapsed time using this class instead of direct calls to {@link
	/// System#nanoTime} for a few reasons:
	/// 
	/// <ul>
	/// <li>An alternate time source can be substituted, for testing or performance
	///     reasons.
	/// <li>As documented by {@code nanoTime}, the value returned has no absolute
	///     meaning, and can only be interpreted as relative to another timestamp
	///     returned by {@code nanoTime} at a different time. {@code Stopwatch} is a
	///     more effective abstraction because it exposes only these relative values,
	///     not the absolute ones.
	/// </ul>
	/// 
	/// <para>Basic usage:
	/// <pre>
	///   Stopwatch stopwatch = Stopwatch.<seealso cref="#createStarted createStarted"/>();
	///   doSomething();
	///   stopwatch.<seealso cref="#stop stop"/>(); // optional
	/// 
	///   long millis = stopwatch.elapsed(MILLISECONDS);
	/// 
	///   log.info("time: " + stopwatch); // formatted string like "12.3 ms"</pre>
	/// 
	/// </para>
	/// <para>Stopwatch methods are not idempotent; it is an error to start or stop a
	/// stopwatch that is already in the desired state.
	/// 
	/// </para>
	/// <para>When testing code that uses this class, use
	/// <seealso cref="#createUnstarted(Ticker)"/> or <seealso cref="#createStarted(Ticker)"/> to
	/// supply a fake or mock ticker.
	/// <!-- TODO(kevinb): restore the "such as" --> This allows you to
	/// simulate any valid behavior of the stopwatch.
	/// 
	/// </para>
	/// <para><b>Note:</b> This class is not thread-safe.
	/// 
	/// @author Kevin Bourrillion
	/// @since 10.0
	/// </para>
	/// </summary>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @Beta @GwtCompatible(emulated = true) public final class Stopwatch
	public sealed class Stopwatch
	{
	  private readonly Ticker ticker;
	  private bool isRunning;
	  private long elapsedNanos_Renamed;
	  private long startTick;

	  /// <summary>
	  /// Creates (but does not start) a new stopwatch using <seealso cref="System#nanoTime"/>
	  /// as its time source.
	  /// 
	  /// @since 15.0
	  /// </summary>
	  public static Stopwatch createUnstarted()
	  {
		return new Stopwatch();
	  }

	  /// <summary>
	  /// Creates (but does not start) a new stopwatch, using the specified time
	  /// source.
	  /// 
	  /// @since 15.0
	  /// </summary>
	  public static Stopwatch createUnstarted(Ticker ticker)
	  {
		return new Stopwatch(ticker);
	  }

	  /// <summary>
	  /// Creates (and starts) a new stopwatch using <seealso cref="System#nanoTime"/>
	  /// as its time source.
	  /// 
	  /// @since 15.0
	  /// </summary>
	  public static Stopwatch createStarted()
	  {
		return (new Stopwatch()).start();
	  }

	  /// <summary>
	  /// Creates (and starts) a new stopwatch, using the specified time
	  /// source.
	  /// 
	  /// @since 15.0
	  /// </summary>
	  public static Stopwatch createStarted(Ticker ticker)
	  {
		return (new Stopwatch(ticker)).start();
	  }

	  /// <summary>
	  /// Creates (but does not start) a new stopwatch using <seealso cref="System#nanoTime"/>
	  /// as its time source.
	  /// </summary>
	  /// @deprecated Use <seealso cref="Stopwatch#createUnstarted()"/> instead. 
      internal Stopwatch()
          : this(Ticker.systemTicker())
	  {
	  }

	  /// <summary>
	  /// Creates (but does not start) a new stopwatch, using the specified time
	  /// source.
	  /// </summary>
	  /// @deprecated Use <seealso cref="Stopwatch#createUnstarted(Ticker)"/> instead. 
	  internal Stopwatch(Ticker ticker)
	  {
		this.ticker = Preconditions.checkNotNull(ticker, "ticker");
	  }

	  /// <summary>
	  /// Returns {@code true} if <seealso cref="#start()"/> has been called on this stopwatch,
	  /// and <seealso cref="#stop()"/> has not been called since the last call to {@code
	  /// start()}.
	  /// </summary>
	  public bool Running
	  {
		  get
		  {
			return isRunning;
		  }
	  }

	  /// <summary>
	  /// Starts the stopwatch.
	  /// </summary>
	  /// <returns> this {@code Stopwatch} instance </returns>
	  /// <exception cref="IllegalStateException"> if the stopwatch is already running. </exception>
	  public Stopwatch start()
	  {
		Preconditions.checkState(!isRunning, "This stopwatch is already running.");
		isRunning = true;
		startTick = ticker.read();
		return this;
	  }

	  /// <summary>
	  /// Stops the stopwatch. Future reads will return the fixed duration that had
	  /// elapsed up to this point.
	  /// </summary>
	  /// <returns> this {@code Stopwatch} instance </returns>
	  /// <exception cref="IllegalStateException"> if the stopwatch is already stopped. </exception>
	  public Stopwatch stop()
	  {
		long tick = ticker.read();
		Preconditions.checkState(isRunning, "This stopwatch is already stopped.");
		isRunning = false;
		elapsedNanos_Renamed += tick - startTick;
		return this;
	  }

	  /// <summary>
	  /// Sets the elapsed time for this stopwatch to zero,
	  /// and places it in a stopped state.
	  /// </summary>
	  /// <returns> this {@code Stopwatch} instance </returns>
	  public Stopwatch reset()
	  {
		elapsedNanos_Renamed = 0;
		isRunning = false;
		return this;
	  }

	  private long elapsedNanos()
	  {
		return isRunning ? ticker.read() - startTick + elapsedNanos_Renamed : elapsedNanos_Renamed;
	  }

	  /// <summary>
	  /// Returns the current elapsed time shown on this stopwatch, expressed
	  /// in the desired time unit, with any fraction rounded down.
	  /// 
	  /// <para>Note that the overhead of measurement can be more than a microsecond, so
	  /// it is generally not useful to specify <seealso cref="TimeUnit#NANOSECONDS"/>
	  /// precision here.
	  /// 
	  /// @since 14.0 (since 10.0 as {@code elapsedTime()})
	  /// </para>
	  /// </summary>
      public long elapsedMicros()
	  {
		return elapsedNanos()/1000;
	  }

	  /// <summary>
	  /// Returns a string representation of the current elapsed time.
	  /// </summary>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @GwtIncompatible("String.format()") @Override public String toString()
	  public override string ToString()
	  {
		long nanos = elapsedNanos();

		// Too bad this functionality is not exposed as a regular method call
        return string.Format("{0:g4} {1}", nanos, "ns");
	  }
	}

}