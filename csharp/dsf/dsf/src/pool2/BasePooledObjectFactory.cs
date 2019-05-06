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
    /// A base implementation of <code>PoolableObjectFactory</code>.
    /// <para>
    /// All operations defined here are essentially no-op's.
    /// </para>
    /// <para>
    /// This class is immutable, and therefore thread-safe
    /// 
    /// </para>
    /// </summary>
    /// @param <T> Type of element managed in this factory.
    /// </param>
    /// <seealso cref= PooledObjectFactory </seealso>
    /// <seealso cref= BaseKeyedPooledObjectFactory
    /// 
    /// @version $Revision: 1333925 $
    /// 
    /// @since 2.0 </seealso>
    public abstract class BasePooledObjectFactory<T> : PooledObjectFactory<T>
    {
        /// <summary>
        /// Creates an object instance, to be wrapped in a <seealso cref="PooledObject"/>.
        /// <para>This method <strong>must</strong> support concurrent, multi-threaded
        /// activation.</para>
        /// </summary>
        /// <returns> an instance to be served by the pool </returns>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: public abstract T create() throws Exception;
        public abstract T create();

        /// <summary>
        /// Wrap the provided instance with an implementation of
        /// <seealso cref="PooledObject"/>.
        /// </summary>
        /// <param name="obj"> the instance to wrap
        /// </param>
        /// <returns> The provided instance, wrapped by a <seealso cref="PooledObject"/> </returns>
        public abstract PooledObject<T> wrap(T obj);

//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public PooledObject<T> makeObject() throws Exception
        public PooledObject<T> makeObject()
        {
            return wrap(create());
        }

        /// <summary>
        ///  No-op.
        /// </summary>
        ///  <param name="p"> ignored </param>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void destroyObject(PooledObject<T> p) throws Exception
        public virtual void destroyObject(PooledObject<T> p)
        {
        }

        /// <summary>
        /// This implementation always returns {@code true}.
        /// </summary>
        /// <param name="p"> ignored
        /// </param>
        /// <returns> {@code true} </returns>
        public virtual bool validateObject(PooledObject<T> p)
        {
            return true;
        }

        /// <summary>
        ///  No-op.
        /// </summary>
        ///  <param name="p"> ignored </param>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void activateObject(PooledObject<T> p) throws Exception
        public void activateObject(PooledObject<T> p)
        {
        }

        /// <summary>
        ///  No-op.
        /// </summary>
        /// <param name="p"> ignored </param>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: @Override public void passivateObject(PooledObject<T> p) throws Exception
        public void passivateObject(PooledObject<T> p)
        {
        }
    }

}