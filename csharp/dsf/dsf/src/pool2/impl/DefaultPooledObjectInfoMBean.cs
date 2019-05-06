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

    /// <summary>
    /// The interface that defines the information about pooled objects that will be
    /// exposed via JMX.
    /// 
    /// NOTE: This interface is subject to change between major version releases
    /// of commons pool.
    /// 
    /// @since 2.0
    /// </summary>
    public interface DefaultPooledObjectInfoMBean
    {
        /// <summary>
        /// Obtain the time (using the same basis as
        /// <seealso cref="System#currentTimeMillis()"/>) that pooled object was created.
        /// </summary>
        /// <returns> The creation time for the pooled object </returns>
        long CreateTime {get;}

        /// <summary>
        /// Obtain the time that pooled object was created.
        /// </summary>
        /// <returns> The creation time for the pooled object formated as
        ///         <code>yyyy-MM-dd HH:mm:ss Z</code> </returns>
        string CreateTimeFormatted {get;}

        /// <summary>
        /// Obtain the time (using the same basis as
        /// <seealso cref="System#currentTimeMillis()"/>) the polled object was last borrowed.
        /// </summary>
        /// <returns> The time the pooled object was last borrowed </returns>
        long LastBorrowTime {get;}

        /// <summary>
        /// Obtain the time that pooled object was last borrowed.
        /// </summary>
        /// <returns> The last borrowed time for the pooled object formated as
        ///         <code>yyyy-MM-dd HH:mm:ss Z</code> </returns>
        string LastBorrowTimeFormatted {get;}

        /// <summary>
        /// Obtain the stack trace recorded when the pooled object was last borrowed.
        /// </summary>
        /// <returns> The stack trace showing which code last borrowed the pooled
        ///         object </returns>
        string LastBorrowTrace {get;}


        /// <summary>
        /// Obtain the time (using the same basis as
        /// <seealso cref="System#currentTimeMillis()"/>)the wrapped object was last returned.
        /// </summary>
        /// <returns> The time the object was last returned </returns>
        long LastReturnTime {get;}

        /// <summary>
        /// Obtain the time that pooled object was last returned.
        /// </summary>
        /// <returns> The last returned time for the pooled object formated as
        ///         <code>yyyy-MM-dd HH:mm:ss Z</code> </returns>
        string LastReturnTimeFormatted {get;}

        /// <summary>
        /// Obtain the name of the class of the pooled object.
        /// </summary>
        /// <returns> The pooled object's class name
        /// </returns>
        /// <seealso cref= Class#getName() </seealso>
        string PooledObjectType {get;}

        /// <summary>
        /// Provides a String form of the wrapper for debug purposes. The format is
        /// not fixed and may change at any time.
        /// </summary>
        /// <returns> A string representation of the pooled object
        /// </returns>
        /// <seealso cref= Object#toString() </seealso>
        string PooledObjectToString {get;}

        /// <summary>
        /// Get the number of times this object has been borrowed. </summary>
        /// <returns> The number of times this object has been borrowed. </returns>
        long BorrowedCount {get;}
    }

}