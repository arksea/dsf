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
    /// A pooling simple interface.
    /// <para>
    /// Example of use:
    /// <pre style="border:solid thin; padding: 1ex;"
    /// > Object obj = <code style="color:#00C">null</code>;
    /// 
    /// <code style="color:#00C">try</code> {
    ///     obj = pool.borrowObject();
    ///     <code style="color:#00C">try</code> {
    ///         <code style="color:#0C0">//...use the object...</code>
    ///     } <code style="color:#00C">catch</code>(Exception e) {
    ///         <code style="color:#0C0">// invalidate the object</code>
    ///         pool.invalidateObject(obj);
    ///         <code style="color:#0C0">// do not return the object to the pool twice</code>
    ///         obj = <code style="color:#00C">null</code>;
    ///     } <code style="color:#00C">finally</code> {
    ///         <code style="color:#0C0">// make sure the object is returned to the pool</code>
    ///         <code style="color:#00C">if</code>(<code style="color:#00C">null</code> != obj) {
    ///             pool.returnObject(obj);
    ///        }
    ///     }
    /// } <code style="color:#00C">catch</code>(Exception e) {
    ///       <code style="color:#0C0">// failed to borrow an object</code>
    /// }</pre>
    /// </para>
    /// <para>
    /// See <seealso cref="BaseObjectPool"/> for a simple base implementation.
    /// 
    /// </para>
    /// </summary>
    /// @param <T> Type of element pooled in this pool.
    /// </param>
    /// <seealso cref= PooledObjectFactory </seealso>
    /// <seealso cref= KeyedObjectPool </seealso>
    /// <seealso cref= BaseObjectPool
    /// 
    /// @version $Revision: 1533126 $
    /// 
    /// @since 2.0 </seealso>
    public interface ObjectPool<T>
    {
        /// <summary>
        /// Obtains an instance from this pool.
        /// <para>
        /// Instances returned from this method will have been either newly created
        /// with <seealso cref="PooledObjectFactory#makeObject"/> or will be a previously
        /// idle object and have been activated with
        /// <seealso cref="PooledObjectFactory#activateObject"/> and then validated with
        /// <seealso cref="PooledObjectFactory#validateObject"/>.
        /// </para>
        /// <para>
        /// By contract, clients <strong>must</strong> return the borrowed instance
        /// using <seealso cref="#returnObject"/>, <seealso cref="#invalidateObject"/>, or a related
        /// method as defined in an implementation or sub-interface.
        /// </para>
        /// <para>
        /// The behaviour of this method when the pool has been exhausted
        /// is not strictly specified (although it may be specified by
        /// implementations).
        /// 
        /// </para>
        /// </summary>
        /// <returns> an instance from this pool.
        /// </returns>
        /// <exception cref="IllegalStateException">
        ///              after <seealso cref="#close close"/> has been called on this pool. </exception>
        /// <exception cref="Exception">
        ///              when <seealso cref="PooledObjectFactory#makeObject"/> throws an
        ///              exception. </exception>
        /// <exception cref="NoSuchElementException">
        ///              when the pool is exhausted and cannot or will not return
        ///              another instance. </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: T borrowObject() throws Exception, java.util.NoSuchElementException, IllegalStateException;
        T borrowObject();

        /// <summary>
        /// Return an instance to the pool. By contract, <code>obj</code>
        /// <strong>must</strong> have been obtained using <seealso cref="#borrowObject()"/> or
        /// a related method as defined in an implementation or sub-interface.
        /// </summary>
        /// <param name="obj"> a <seealso cref="#borrowObject borrowed"/> instance to be returned.
        /// </param>
        /// <exception cref="Exception"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void returnObject(T obj) throws Exception;
        void returnObject(T obj);

        /// <summary>
        /// Invalidates an object from the pool.
        /// <para>
        /// By contract, <code>obj</code> <strong>must</strong> have been obtained
        /// using <seealso cref="#borrowObject"/> or a related method as defined in an
        /// implementation or sub-interface.
        /// </para>
        /// <para>
        /// This method should be used when an object that has been borrowed is
        /// determined (due to an exception or other problem) to be invalid.
        /// 
        /// </para>
        /// </summary>
        /// <param name="obj"> a <seealso cref="#borrowObject borrowed"/> instance to be disposed.
        /// </param>
        /// <exception cref="Exception"> </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void invalidateObject(T obj) throws Exception;
        void invalidateObject(T obj);

        /// <summary>
        /// Create an object using the <seealso cref="PooledObjectFactory factory"/> or other
        /// implementation dependent mechanism, passivate it, and then place it in
        /// the idle object pool. <code>addObject</code> is useful for "pre-loading"
        /// a pool with idle objects. (Optional operation).
        /// </summary>
        /// <exception cref="Exception">
        ///              when <seealso cref="PooledObjectFactory#makeObject"/> fails. </exception>
        /// <exception cref="IllegalStateException">
        ///              after <seealso cref="#close"/> has been called on this pool. </exception>
        /// <exception cref="UnsupportedOperationException">
        ///              when this pool cannot add new idle objects. </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void addObject() throws Exception, IllegalStateException, UnsupportedOperationException;
        void addObject();

        /// <summary>
        /// Return the number of instances currently idle in this pool. This may be
        /// considered an approximation of the number of objects that can be
        /// <seealso cref="#borrowObject borrowed"/> without creating any new instances.
        /// Returns a negative value if this information is not available. </summary>
        /// <returns> the number of instances currently idle in this pool. </returns>
        int NumIdle {get;}

        /// <summary>
        /// Return the number of instances currently borrowed from this pool. Returns
        /// a negative value if this information is not available. </summary>
        /// <returns> the number of instances currently borrowed from this pool. </returns>
        int NumActive {get;}

        /// <summary>
        /// Clears any objects sitting idle in the pool, releasing any associated
        /// resources (optional operation). Idle objects cleared must be
        /// <seealso cref="PooledObjectFactory#destroyObject(PooledObject)"/>.
        /// </summary>
        /// <exception cref="UnsupportedOperationException">
        ///              if this implementation does not support the operation </exception>
//JAVA TO C# CONVERTER WARNING: Method 'throws' clauses are not available in .NET:
//ORIGINAL LINE: void clear() throws Exception, UnsupportedOperationException;
        void clear();

        /// <summary>
        /// Close this pool, and free any resources associated with it.
        /// <para>
        /// Calling <seealso cref="#addObject"/> or <seealso cref="#borrowObject"/> after invoking this
        /// method on a pool will cause them to throw an <seealso cref="IllegalStateException"/>.
        /// </para>
        /// <para>
        /// Implementations should silently fail if not all resources can be freed.
        /// </para>
        /// </summary>
        void close();
    }

}