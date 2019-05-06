using System.Collections.Generic;
using System.Threading;

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
    using java.util.concurrent.locks;

    /// <summary>
    /// This sub-class was created to expose the waiting threads so that they can be
    /// interrupted when the pool using the queue that uses this lock is closed. The
    /// class is intended for internal use only.
    /// <para>
    /// This class is intended to be thread-safe.
    /// 
    /// @since 2.0
    /// </para>
    /// </summary>
    internal class InterruptibleReentrantLock : ReentrantLock
    {

        private const long serialVersionUID = 1L;

        /// <summary>
        /// Interrupt the threads that are waiting on a specific condition
        /// </summary>
        /// <param name="condition"> the condition on which the threads are waiting. </param>
        public virtual void interruptWaiters(Condition condition)
        {
            java.util.Collection threads = getWaitingThreads(condition);
            java.util.Iterator it = threads.iterator();
            while(it.hasNext())
            {
                java.lang.Thread thread = (java.lang.Thread)it.next();
                thread.interrupt();
            }
        }
    }

}