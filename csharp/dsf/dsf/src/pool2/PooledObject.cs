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


	/// <summary>
	/// Defines the wrapper that is used to track the additional information, such as
	/// state, for the pooled objects.
	/// <para>
	/// Implementations of this class are required to be thread-safe.
	/// 
	/// </para>
	/// </summary>
	/// @param <T> the type of object in the pool
	/// 
	/// @version $Revision: $
	/// 
	/// @since 2.0 </param>
	public interface PooledObject<T> : System.IComparable<PooledObject<T>>
	{

		/// <summary>
		/// Obtain the underlying object that is wrapped by this instance of
		/// <seealso cref="PooledObject"/>.
		/// </summary>
		/// <returns> The wrapped object </returns>
		T Object {get;}

		/// <summary>
		/// Obtain the time (using the same basis as
		/// <seealso cref="System#currentTimeMillis()"/>) that this object was created.
		/// </summary>
		/// <returns> The creation time for the wrapped object </returns>
		long CreateTime {get;}

		/// <summary>
		/// Obtain the time in milliseconds that this object last spent in the the
		/// active state (it may still be active in which case subsequent calls will
		/// return an increased value).
		/// </summary>
		/// <returns> The time in milliseconds last spent in the active state </returns>
		long ActiveTimeMillis {get;}

		/// <summary>
		/// Obtain the time in milliseconds that this object last spend in the the
		/// idle state (it may still be idle in which case subsequent calls will
		/// return an increased value).
		/// </summary>
		/// <returns> The time in milliseconds last spent in the idle state </returns>
		long IdleTimeMillis {get;}

		/// <summary>
		/// Obtain the time the wrapped object was last borrowed.
		/// </summary>
		/// <returns> The time the object was last borrowed </returns>
		long LastBorrowTime {get;}

		/// <summary>
		/// Obtain the time the wrapped object was last returned.
		/// </summary>
		/// <returns> The time the object was last returned </returns>
		long LastReturnTime {get;}

		/// <summary>
		/// Return an estimate of the last time this object was used.  If the class
		/// of the pooled object implements <seealso cref="TrackedUse"/>, what is returned is
		/// the maximum of <seealso cref="TrackedUse#getLastUsed()"/> and
		/// <seealso cref="#getLastBorrowTime()"/>; otherwise this method gives the same
		/// value as <seealso cref="#getLastBorrowTime()"/>.
		/// </summary>
		/// <returns> the last time this object was used </returns>
		long LastUsedTime {get;}

		/// <summary>
		/// Orders instances based on idle time - i.e. the length of time since the
		/// instance was returned to the pool. Used by the GKOP idle object evictor.
		/// <para>
		/// Note: This class has a natural ordering that is inconsistent with
		///       equals if distinct objects have the same identity hash code.
		/// </para>
		/// <para>
		/// {@inheritDoc}
		/// </para>
		/// </summary>

		bool Equals(object obj);

		int GetHashCode();

		/// <summary>
		/// Provides a String form of the wrapper for debug purposes. The format is
		/// not fixed and may change at any time.
		/// <para>
		/// {@inheritDoc}
		/// </para>
		/// </summary>
		string ToString();

		/// <summary>
		/// Attempt to place the pooled object in the
		/// <seealso cref="PooledObjectState#EVICTION"/> state.
		/// </summary>
		/// <returns> <code>true</code> if the object was placed in the
		///         <seealso cref="PooledObjectState#EVICTION"/> state otherwise
		///         <code>false</code> </returns>
		bool startEvictionTest();

		/// <summary>
		/// Called to inform the object that the eviction test has ended.
		/// </summary>
		/// <param name="idleQueue"> The queue if idle objects to which the object should be
		///                  returned
		/// </param>
		/// <returns>  Currently not used </returns>
		bool endEvictionTest(java.util.Deque idleQueue);

		/// <summary>
		/// Allocates the object.
		/// </summary>
		/// <returns> {@code true} if the original state was <seealso cref="PooledObjectState#IDLE IDLE"/> </returns>
		bool allocate();

		/// <summary>
		/// Deallocates the object and sets it <seealso cref="PooledObjectState#IDLE IDLE"/>
		/// if it is currently <seealso cref="PooledObjectState#ALLOCATED ALLOCATED"/>.
		/// </summary>
		/// <returns> {@code true} if the state was <seealso cref="PooledObjectState#ALLOCATED ALLOCATED"/> </returns>
		bool deallocate();

		/// <summary>
		/// Sets the state to <seealso cref="PooledObjectState#INVALID INVALID"/>
		/// </summary>
		void invalidate();

		/// <summary>
		/// Is abandoned object tracking being used? If this is true the
		/// implementation will need to record the stack trace of the last caller to
		/// borrow this object.
		/// </summary>
		/// <param name="logAbandoned">    The new configuration setting for abandoned
		///                          object tracking </param>
		bool LogAbandoned {set;}

		/// <summary>
		/// Record the current stack trace as the last time the object was used.
		/// </summary>
		void use();

		/// <summary>
		/// Prints the stack trace of the code that borrowed this pooled object and
		/// the stack trace of the last code to use this object (if available) to
		/// the supplied writer.
		/// </summary>
		/// <param name="writer">  The destination for the debug output </param>
		void printStackTrace(java.io.PrintWriter writer);

		/// <summary>
		/// Returns the state of this object. </summary>
		/// <returns> state </returns>
		PooledObjectState State {get;}

		/// <summary>
		/// Marks the pooled object as abandoned.
		/// </summary>
		void markAbandoned();

		/// <summary>
		/// Marks the object as returning to the pool.
		/// </summary>
		void markReturning();

		// TODO: Uncomment this for version 3 (can't add it to 2.x as it will break
		//       API compatibility)
		///**
		// * Get the number of times this object has been borrowed.
		// */
		//long getBorrowedCount();
	}

}