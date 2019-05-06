using System;
using System.Text;

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

    /// <summary>
    /// This wrapper is used to track the additional information, such as state, for
    /// the pooled objects.
    /// <para>
    /// This class is intended to be thread-safe.
    /// 
    /// </para>
    /// </summary>
    /// @param <T> the type of object in the pool
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0 </param>
    public class DefaultPooledObject<T> : PooledObject<T>
    {
        private bool InstanceFieldsInitialized = false;

        private void InitializeInstanceFields()
        {
            lastBorrowTime = new AtomicLong(createTime);
            lastUseTime = new AtomicLong(createTime);
            lastReturnTime = new AtomicLong(createTime);
            borrowedCount = new AtomicLong(0L);
        }


        private readonly T @object;
        private PooledObjectState state = PooledObjectState.IDLE; // @GuardedBy("this") to ensure transitions are valid
        private readonly long createTime = java.lang.System.currentTimeMillis();
        private AtomicLong lastBorrowTime;
        private AtomicLong lastUseTime;
        private AtomicLong lastReturnTime;
        private volatile bool logAbandoned = false;
        private volatile Exception borrowedBy = null;
        private volatile Exception usedBy = null;
        private AtomicLong borrowedCount;

        /// <summary>
        /// Create a new instance that wraps the provided object so that the pool can
        /// track the state of the pooled object.
        /// </summary>
        /// <param name="object"> The object to wrap </param>
        public DefaultPooledObject(T @object)
        {
            if (!InstanceFieldsInitialized)
            {
                InitializeInstanceFields();
                InstanceFieldsInitialized = true;
            }
            this.@object = @object;
        }

        public T Object
        {
            get
            {
                return @object;
            }
        }

        public long CreateTime
        {
            get
            {
                return createTime;
            }
        }

        public long ActiveTimeMillis
        {
            get
            {
                // Take copies to avoid threading issues
                long rTime = lastReturnTime.get();
                long bTime = lastBorrowTime.get();
    
                if (rTime > bTime)
                {
                    return rTime - bTime;
                }
                else
                {
                    return java.lang.System.currentTimeMillis() - bTime;
                }
            }
        }

        public long IdleTimeMillis
        {
            get
            {
                return java.lang.System.currentTimeMillis() - lastReturnTime.get();
            }
        }

        public long LastBorrowTime
        {
            get
            {
                return lastBorrowTime.get();
            }
        }

        public long LastReturnTime
        {
            get
            {
                return lastReturnTime.get();
            }
        }

        /// <summary>
        /// Get the number of times this object has been borrowed. </summary>
        /// <returns> The number of times this object has been borrowed. </returns>
        public virtual long BorrowedCount
        {
            get
            {
                return borrowedCount.get();
            }
        }

        /// <summary>
        /// Return an estimate of the last time this object was used.  If the class
        /// of the pooled object implements <seealso cref="TrackedUse"/>, what is returned is
        /// the maximum of <seealso cref="TrackedUse#getLastUsed()"/> and
        /// <seealso cref="#getLastBorrowTime()"/>; otherwise this method gives the same
        /// value as <seealso cref="#getLastBorrowTime()"/>.
        /// </summary>
        /// <returns> the last time this object was used </returns>
        public long LastUsedTime
        {
            get
            {
                long t;
                lock(lastUseTime)
                {
                    t = lastUseTime.get();
                }
                if (@object is TrackedUse)
                {
                    return Math.Max(((TrackedUse) @object).LastUsed, t);
                }
                else
                {
                    return t;
                }
            }
        }

        public virtual int CompareTo(PooledObject<T> other)
        {
//JAVA TO C# CONVERTER WARNING: The original Java variable was marked 'final':
//ORIGINAL LINE: final long lastActiveDiff = this.getLastReturnTime() - other.getLastReturnTime();
            long lastActiveDiff = this.LastReturnTime - other.LastReturnTime;
            if (lastActiveDiff == 0)
            {
                // Make sure the natural ordering is broadly consistent with equals
                // although this will break down if distinct objects have the same
                // identity hash code.
                // see java.lang.Comparable Javadocs
                return java.lang.System.identityHashCode(this) - java.lang.System.identityHashCode(other);
            }
            // handle int overflow
            return (int)Math.Min(Math.Max(lastActiveDiff, int.MinValue), int.MaxValue);
        }

        public override string ToString()
        {
            StringBuilder result = new StringBuilder();
            result.Append("Object: ");
            result.Append(@object.ToString());
            result.Append(", State: ");
            lock (this)
            {
                result.Append(state.ToString());
            }
            return result.ToString();
            // TODO add other attributes
        }

        public bool startEvictionTest()
        {
            lock (this)
            {
                if (state == PooledObjectState.IDLE)
                {
                    state = PooledObjectState.EVICTION;
                    return true;
                }
        
                return false;
            }
        }

        public bool endEvictionTest(java.util.Deque idleQueue)
        {
            lock (this)
            {
                if (state == PooledObjectState.EVICTION)
                {
                    state = PooledObjectState.IDLE;
                    return true;
                }
                else if (state == PooledObjectState.EVICTION_RETURN_TO_HEAD)
                {
                    state = PooledObjectState.IDLE;
                    if (!idleQueue.offerFirst(this))
                    {
                        // TODO - Should never happen
                    }
                }
        
                return false;
            }
        }

        /// <summary>
        /// Allocates the object.
        /// </summary>
        /// <returns> {@code true} if the original state was <seealso cref="PooledObjectState#IDLE IDLE"/> </returns>
        public bool allocate()
        {
            lock (this)
            {
                if (state == PooledObjectState.IDLE)
                {
                    state = PooledObjectState.ALLOCATED;
                    lastBorrowTime.set(java.lang.System.currentTimeMillis());
                    lastUseTime = lastBorrowTime;
                    borrowedCount.incrementAndGet();
                    if (logAbandoned)
                    {
                        borrowedBy = new AbandonedObjectCreatedException();
                    }
                    return true;
                }
                else if (state == PooledObjectState.EVICTION)
                {
                    // TODO Allocate anyway and ignore eviction test
                    state = PooledObjectState.EVICTION_RETURN_TO_HEAD;
                    return false;
                }
                // TODO if validating and testOnBorrow == true then pre-allocate for
                // performance
                return false;
            }
        }

        /// <summary>
        /// Deallocates the object and sets it <seealso cref="PooledObjectState#IDLE IDLE"/>
        /// if it is currently <seealso cref="PooledObjectState#ALLOCATED ALLOCATED"/>.
        /// </summary>
        /// <returns> {@code true} if the state was <seealso cref="PooledObjectState#ALLOCATED ALLOCATED"/> </returns>
        public bool deallocate()
        {
            lock (this)
            {
                if (state == PooledObjectState.ALLOCATED || state == PooledObjectState.RETURNING)
                {
                    state = PooledObjectState.IDLE;
                    lastReturnTime.set(java.lang.System.currentTimeMillis());
                    if (borrowedBy != null)
                    {
                        borrowedBy = null;
                    }
                    return true;
                }
        
                return false;
            }
        }

        /// <summary>
        /// Sets the state to <seealso cref="PooledObjectState#INVALID INVALID"/>
        /// </summary>
        public void invalidate()
        {
            lock (this)
            {
                state = PooledObjectState.INVALID;
            }
        }

        public void use()
        {
            lastUseTime.set(java.lang.System.currentTimeMillis());
            usedBy = new Exception("The last code to use this object was:");
        }

        public void printStackTrace(java.io.PrintWriter writer)
        {
            Exception borrowedByCopy = this.borrowedBy;
            if (borrowedByCopy != null)
            {
                writer.println(borrowedByCopy.Message);
                writer.print(borrowedByCopy.StackTrace);
            }
            Exception usedByCopy = this.usedBy;
            if (usedByCopy != null)
            {
                writer.println(usedByCopy.Message);
                writer.print(usedByCopy.StackTrace);
            }
        }

        /// <summary>
        /// Returns the state of this object. </summary>
        /// <returns> state </returns>
        public PooledObjectState State
        {
            get
            {
                lock (this)
                {
                    return state;
                }
            }
        }

        /// <summary>
        /// Marks the pooled object as abandoned.
        /// </summary>
        public void markAbandoned()
        {
            lock (this)
            {
                state = PooledObjectState.ABANDONED;
            }
        }

        /// <summary>
        /// Marks the object as returning to the pool.
        /// </summary>
        public void markReturning()
        {
            lock (this)
            {
                state = PooledObjectState.RETURNING;
            }
        }

        public bool LogAbandoned
        {
            set
            {
                this.logAbandoned = value;
            }
        }

        /// <summary>
        /// Used to track how an object was obtained from the pool (the stack trace
        /// of the exception will show which code borrowed the object) and when the
        /// object was borrowed.
        /// </summary>
        internal class AbandonedObjectCreatedException : Exception
        {

            internal const long serialVersionUID = 7398692158058772916L;

            /// <summary>
            /// Date format </summary>
            //@GuardedBy("this")
            internal static readonly java.text.SimpleDateFormat format = new java.text.SimpleDateFormat("'Pooled object created' yyyy-MM-dd HH:mm:ss Z " + "'by the following code has not been returned to the pool:'");

            internal readonly long _createdTime;

            /// <summary>
            /// Create a new instance.
            /// <para>
            /// </para>
            /// </summary>
            /// <seealso cref= Exception#Exception() </seealso>
            public AbandonedObjectCreatedException() : base()
            {
                _createdTime = java.lang.System.currentTimeMillis();
            }

            // Override getMessage to avoid creating objects and formatting
            // dates unless the log message will actually be used.
            public override string Message
            {
                get
                {
                    string msg;
                    lock (format)
                    {
                        msg = format.format(new DateTime(_createdTime));
                    }
                    return msg;
                }
            }
        }
    }

}