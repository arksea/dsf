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
    /// An interface defining life-cycle methods for instances to be served by an
    /// <seealso cref="ObjectPool"/>.
    /// <para>
    /// By contract, when an <seealso cref="ObjectPool"/> delegates to a
    /// <seealso cref="PooledObjectFactory"/>,
    /// <ol>
    ///  <li>
    ///   <seealso cref="#makeObject"/> is called whenever a new instance is needed.
    ///  </li>
    ///  <li>
    ///   <seealso cref="#activateObject"/> is invoked on every instance that has been
    ///   <seealso cref="#passivateObject passivated"/> before it is
    ///   <seealso cref="ObjectPool#borrowObject borrowed"/> from the pool.
    ///  </li>
    ///  <li>
    ///   <seealso cref="#validateObject"/> may be invoked on <seealso cref="#activateObject activated"/>
    ///   instances to make sure they can be <seealso cref="ObjectPool#borrowObject borrowed"/>
    ///   from the pool. <seealso cref="#validateObject"/> may also be used to
    ///   test an instance being <seealso cref="ObjectPool#returnObject returned"/> to the pool
    ///   before it is <seealso cref="#passivateObject passivated"/>. It will only be invoked
    ///   on an activated instance.
    ///  </li>
    ///  <li>
    ///   <seealso cref="#passivateObject"/> is invoked on every instance when it is returned
    ///   to the pool.
    ///  </li>
    ///  <li>
    ///   <seealso cref="#destroyObject"/> is invoked on every instance when it is being
    ///   "dropped" from the pool (whether due to the response from
    ///   <seealso cref="#validateObject"/>, or for reasons specific to the pool
    ///   implementation.) There is no guarantee that the instance being destroyed
    ///   will be considered active, passive or in a generally consistent state.
    ///  </li>
    /// </ol>
    /// <seealso cref="PooledObjectFactory"/> must be thread-safe. The only promise
    /// an <seealso cref="ObjectPool"/> makes is that the same instance of an object will not
    /// be passed to more than one method of a <code>PoolableObjectFactory</code>
    /// at a time.
    /// </para>
    /// <para>
    /// While clients of a <seealso cref="KeyedObjectPool"/> borrow and return instances of
    /// the underlying value type {@code V}, the factory methods act on instances of
    /// <seealso cref="PooledObject PooledObject<V>"/>.  These are the object wrappers that
    /// pools use to track and maintain state information about the objects that
    /// they manage.
    /// 
    /// </para>
    /// </summary>
    /// @param <T> Type of element managed in this factory.
    /// </param>
    /// <seealso cref= ObjectPool
    /// 
    /// @version $Revision: 1333925 $
    /// 
    /// @since 2.0 </seealso>
    public interface PooledObjectFactory<T>
    {
      /// <summary>
      /// Create an instance that can be served by the pool and wrap it in a
      /// <seealso cref="PooledObject"/> to be managed by the pool.
      /// </summary>
      /// <returns> a {@code PooledObject} wrapping an instance that can be served by the pool
      /// </returns>
      /// <exception cref="Exception"> if there is a problem creating a new instance,
      ///    this will be propagated to the code requesting an object. </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: PooledObject<T> makeObject() throws Exception;
      PooledObject<T> makeObject();

      /// <summary>
      /// Destroys an instance no longer needed by the pool.
      /// <para>
      /// It is important for implementations of this method to be aware that there
      /// is no guarantee about what state <code>obj</code> will be in and the
      /// implementation should be prepared to handle unexpected errors.
      /// </para>
      /// <para>
      /// Also, an implementation must take in to consideration that instances lost
      /// to the garbage collector may never be destroyed.
      /// </para>
      /// </summary>
      /// <param name="p"> a {@code PooledObject} wrapping the instance to be destroyed
      /// </param>
      /// <exception cref="Exception"> should be avoided as it may be swallowed by
      ///    the pool implementation.
      /// </exception>
      /// <seealso cref= #validateObject </seealso>
      /// <seealso cref= ObjectPool#invalidateObject </seealso>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void destroyObject(PooledObject<T> p) throws Exception;
      void destroyObject(PooledObject<T> p);

      /// <summary>
      /// Ensures that the instance is safe to be returned by the pool.
      /// </summary>
      /// <param name="p"> a {@code PooledObject} wrapping the instance to be validated
      /// </param>
      /// <returns> <code>false</code> if <code>obj</code> is not valid and should
      ///         be dropped from the pool, <code>true</code> otherwise. </returns>
      bool validateObject(PooledObject<T> p);

      /// <summary>
      /// Reinitialize an instance to be returned by the pool.
      /// </summary>
      /// <param name="p"> a {@code PooledObject} wrapping the instance to be activated
      /// </param>
      /// <exception cref="Exception"> if there is a problem activating <code>obj</code>,
      ///    this exception may be swallowed by the pool.
      /// </exception>
      /// <seealso cref= #destroyObject </seealso>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void activateObject(PooledObject<T> p) throws Exception;
      void activateObject(PooledObject<T> p);

      /// <summary>
      /// Uninitialize an instance to be returned to the idle object pool.
      /// </summary>
      /// <param name="p"> a {@code PooledObject} wrapping the instance to be passivated
      /// </param>
      /// <exception cref="Exception"> if there is a problem passivating <code>obj</code>,
      ///    this exception may be swallowed by the pool.
      /// </exception>
      /// <seealso cref= #destroyObject </seealso>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void passivateObject(PooledObject<T> p) throws Exception;
      void passivateObject(PooledObject<T> p);
    }

}