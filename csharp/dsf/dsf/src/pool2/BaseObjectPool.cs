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
    /// A simple base implementation of <seealso cref="ObjectPool"/>.
    /// Optional operations are implemented to either do nothing, return a value
    /// indicating it is unsupported or throw <seealso cref="UnsupportedOperationException"/>.
    /// <para>
    /// This class is intended to be thread-safe.
    /// 
    /// </para>
    /// </summary>
    /// @param <T> Type of element pooled in this pool.
    /// 
    /// @version $Revision: 1333925 $
    /// 
    /// @since 2.0 </param>
    public abstract class BaseObjectPool<T> : ObjectPool<T>
    {
        /// <summary>
        /// Obtains an instance from the pool.
        /// </summary>
        /// <returns> an instance from the pool
        /// </returns>
        /// <exception cref="Exception"> if an instance cannot be obtained from the pool </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public abstract T borrowObject() throws Exception;
        public abstract T borrowObject();

        /// <summary>
        /// Returns an instance to the pool.
        /// </summary>
        /// <param name="obj"> instance to return to the pool </param>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public abstract void returnObject(T obj) throws Exception;
        public abstract void returnObject(T obj);

        /// <summary>
        /// Invalidates an object from the pool.
        /// <para>
        /// By contract, <code>obj</code> <strong>must</strong> have been obtained
        /// using <seealso cref="#borrowObject borrowObject"/>.
        /// </para>
        /// <para>
        /// This method should be used when an object that has been borrowed is
        /// determined (due to an exception or other problem) to be invalid.
        /// 
        /// </para>
        /// </summary>
        /// <param name="obj"> a <seealso cref="#borrowObject borrowed"/> instance to be disposed. </param>
        /// <exception cref="Exception"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public abstract void invalidateObject(T obj) throws Exception;
        public abstract void invalidateObject(T obj);

        /// <summary>
        /// Not supported in this base implementation.
        /// </summary>
        /// <returns> a negative value. </returns>
        public int NumIdle
        {
            get
            {
                return -1;
            }
        }

        /// <summary>
        /// Not supported in this base implementation.
        /// </summary>
        /// <returns> a negative value. </returns>
        public int NumActive
        {
            get
            {
                return -1;
            }
        }

        /// <summary>
        /// Not supported in this base implementation.
        /// </summary>
        /// <exception cref="UnsupportedOperationException"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void clear() throws Exception, UnsupportedOperationException
        public void clear()
        {
            throw new System.NotSupportedException();
        }

        /// <summary>
        /// Not supported in this base implementation.Always throws an
        /// <seealso cref="UnsupportedOperationException"/>, subclasses should override this
        /// behavior.
        /// </summary>
        /// <exception cref="UnsupportedOperationException"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void addObject() throws Exception, UnsupportedOperationException
        public void addObject()
        {
            throw new System.NotSupportedException();
        }

        /// <summary>
        /// Close this pool. This affects the behavior of <code>isClosed</code> and
        /// <code>assertOpen</code>.
        /// </summary>
        public void close()
        {
            closed = true;
        }

        /// <summary>
        /// Has this pool instance been closed.
        /// </summary>
        /// <returns> <code>true</code> when this pool has been closed. </returns>
        public bool Closed
        {
            get
            {
                return closed;
            }
        }

        /// <summary>
        /// Throws an <code>IllegalStateException</code> when this pool has been
        /// closed.
        /// </summary>
        /// <exception cref="IllegalStateException"> when this pool has been closed.
        /// </exception>
        /// <seealso cref= #isClosed() </seealso>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: protected final void assertOpen() throws IllegalStateException
        protected internal void assertOpen()
        {
            if (Closed)
            {
                throw new java.lang.IllegalStateException("Pool not open");
            }
        }

        private volatile bool closed = false;
    }

}