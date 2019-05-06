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
    using java.util;
    using java.security;

    /// <summary>
    /// Provides a shared idle object eviction timer for all pools. This class wraps
    /// the standard <seealso cref="Timer"/> and keeps track of how many pools are using it.
    /// If no pools are using the timer, it is canceled. This prevents a thread
    /// being left running which, in application server environments, can lead to
    /// memory leads and/or prevent applications from shutting down or reloading
    /// cleanly.
    /// <para>
    /// This class has package scope to prevent its inclusion in the pool public API.
    /// The class declaration below should *not* be changed to public.
    /// </para>
    /// <para>
    /// This class is intended to be thread-safe.
    /// 
    /// @since 2.0
    /// </para>
    /// </summary>
    internal class EvictionTimer
    {

        /// <summary>
        /// Timer instance </summary>
        private static java.util.Timer _timer; //@GuardedBy("this")

        /// <summary>
        /// Static usage count tracker </summary>
        private static int _usageCount; //@GuardedBy("this")

        /// <summary>
        /// Prevent instantiation </summary>
        private EvictionTimer()
        {
            // Hide the default constructor
        }

        /// <summary>
        /// Add the specified eviction task to the timer. Tasks that are added with a
        /// call to this method *must* call <seealso cref="#cancel(TimerTask)"/> to cancel the
        /// task to prevent memory and/or thread leaks in application server
        /// environments. </summary>
        /// <param name="task">      Task to be scheduled </param>
        /// <param name="delay">     Delay in milliseconds before task is executed </param>
        /// <param name="period">    Time in milliseconds between executions </param>
        internal static void schedule(java.util.TimerTask task, long delay, long period)
        {
            lock (typeof(EvictionTimer))
            {
                if (null == _timer)
                {
                    // Force the new Timer thread to be created with a context class
                    // loader set to the class loader that loaded this library
                    java.lang.ClassLoader ccl = (java.lang.ClassLoader)AccessController.doPrivileged(new PrivilegedGetTccl());
                    try
                    {
                        java.lang.Class clazz = (java.lang.Class)typeof(EvictionTimer);
                        AccessController.doPrivileged(new PrivilegedSetTccl(clazz.getClassLoader()));
                        _timer = new Timer("commons-pool-EvictionTimer", true);
                    }
                    finally
                    {
                        AccessController.doPrivileged(new PrivilegedSetTccl(ccl));
                    }
                }
                _usageCount++;
                _timer.schedule(task, delay, period);
            }
        }

        /// <summary>
        /// Remove the specified eviction task from the timer. </summary>
        /// <param name="task">      Task to be scheduled </param>
        internal static void cancel(TimerTask task)
        {
            lock (typeof(EvictionTimer))
            {
                task.cancel();
                _usageCount--;
                if (_usageCount == 0)
                {
                    _timer.cancel();
                    _timer = null;
                }
            }
        }

        /// <summary>
        /// <seealso cref="PrivilegedAction"/> used to get the ContextClassLoader
        /// </summary>
        private class PrivilegedGetTccl : PrivilegedAction
        {

            /// <summary>
            /// {@inheritDoc}
            /// </summary>
            public object run()
            {
                return java.lang.Thread.currentThread().getContextClassLoader();
            }
        }

        /// <summary>
        /// <seealso cref="PrivilegedAction"/> used to set the ContextClassLoader
        /// </summary>
        private class PrivilegedSetTccl : PrivilegedAction
        {

            /// <summary>
            /// ClassLoader </summary>
            internal readonly java.lang.ClassLoader cl;

            /// <summary>
            /// Create a new PrivilegedSetTccl using the given classloader </summary>
            /// <param name="cl"> ClassLoader to use </param>
            internal PrivilegedSetTccl(java.lang.ClassLoader cl)
            {
                this.cl = cl;
            }

            /// <summary>
            /// {@inheritDoc}
            /// </summary>
            public object run()
            {
                java.lang.Thread.currentThread().setContextClassLoader(cl);
                return null;
            }
        }

    }

}