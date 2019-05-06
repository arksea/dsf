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

    /// <summary>
    /// Configuration settings for abandoned object removal.
    /// 
    /// @version $Revision:$
    /// 
    /// @since 2.0
    /// </summary>
    public class AbandonedConfig
    {

        /// <summary>
        /// Whether or not borrowObject performs abandoned object removal.
        /// </summary>
        private bool removeAbandonedOnBorrow = false;

        /// <summary>
        /// <para>Flag to remove abandoned objects if they exceed the
        /// removeAbandonedTimeout when borrowObject is invoked.</para>
        /// 
        /// <para>The default value is false.</para>
        /// 
        /// <para>If set to true, abandoned objects are removed by borrowObject if
        /// there are fewer than 2 idle objects available in the pool and
        /// <code>getNumActive() > getMaxTotal() - 3</code> </para>
        /// </summary>
        /// <returns> true if abandoned objects are to be removed by borrowObject </returns>
        public virtual bool RemoveAbandonedOnBorrow
        {
            get
            {
                return (this.removeAbandonedOnBorrow);
            }
            set
            {
                this.removeAbandonedOnBorrow = value;
            }
        }


        /// <summary>
        /// Whether or not pool maintenance (evictor) performs abandoned object
        /// removal.
        /// </summary>
        private bool removeAbandonedOnMaintenance = false;

        /// <summary>
        /// <para>Flag to remove abandoned objects if they exceed the
        /// removeAbandonedTimeout when pool maintenance (the "evictor")
        /// runs.</para>
        /// 
        /// <para>The default value is false.</para>
        /// 
        /// <para>If set to true, abandoned objects are removed by the pool
        /// maintenance thread when it runs.  This setting has no effect
        /// unless maintenance is enabled by setting
        /// <seealso cref="GenericObjectPool#getTimeBetweenEvictionRunsMillis() timeBetweenEvictionRunsMillis"/>
        /// to a positive number.</para>
        /// </summary>
        /// <returns> true if abandoned objects are to be removed by the evictor </returns>
        public virtual bool RemoveAbandonedOnMaintenance
        {
            get
            {
                return (this.removeAbandonedOnMaintenance);
            }
            set
            {
                this.removeAbandonedOnMaintenance = value;
            }
        }


        /// <summary>
        /// Timeout in seconds before an abandoned object can be removed.
        /// </summary>
        private int removeAbandonedTimeout = 300;

        /// <summary>
        /// <para>Timeout in seconds before an abandoned object can be removed.</para>
        /// 
        /// <para>The time of most recent use of an object is the maximum (latest) of
        /// <seealso cref="TrackedUse#getLastUsed()"/> (if this class of the object implements
        /// TrackedUse) and the time when the object was borrowed from the pool.</para>
        /// 
        /// <para>The default value is 300 seconds.</para>
        /// </summary>
        /// <returns> the abandoned object timeout in seconds </returns>
        public virtual int RemoveAbandonedTimeout
        {
            get
            {
                return (this.removeAbandonedTimeout);
            }
            set
            {
                this.removeAbandonedTimeout = value;
            }
        }


        /// <summary>
        /// Determines whether or not to log stack traces for application code
        /// which abandoned an object.
        /// </summary>
        private bool logAbandoned = false;

        /// <summary>
        /// Flag to log stack traces for application code which abandoned
        /// an object.
        /// 
        /// Defaults to false.
        /// Logging of abandoned objects adds overhead for every object created
        /// because a stack trace has to be generated.
        /// </summary>
        /// <returns> boolean true if stack trace logging is turned on for abandoned
        /// objects
        ///  </returns>
        public virtual bool LogAbandoned
        {
            get
            {
                return (this.logAbandoned);
            }
            set
            {
                this.logAbandoned = value;
            }
        }


        /// <summary>
        /// PrintWriter to use to log information on abandoned objects.
        /// Use of default system encoding is deliberate.
        /// </summary>
        private java.io.PrintWriter logWriter = new java.io.PrintWriter(java.lang.System.@out);

        /// <summary>
        /// Returns the log writer being used by this configuration to log
        /// information on abandoned objects. If not set, a PrintWriter based on
        /// System.out with the system default encoding is used.
        /// </summary>
        /// <returns> log writer in use </returns>
        public virtual java.io.PrintWriter LogWriter
        {
            get
            {
                return logWriter;
            }
            set
            {
                this.logWriter = value;
            }
        }


        /// <summary>
        /// If the pool implements <seealso cref="UsageTracking"/>, should the pool record a
        /// stack trace every time a method is called on a pooled object and retain
        /// the most recent stack trace to aid debugging of abandoned objects?
        /// </summary>
        private bool useUsageTracking = false;

        /// <summary>
        /// If the pool implements <seealso cref="UsageTracking"/>, should the pool record a
        /// stack trace every time a method is called on a pooled object and retain
        /// the most recent stack trace to aid debugging of abandoned objects?
        /// </summary>
        /// <returns> <code>true</code> if usage tracking is enabled </returns>
        public virtual bool UseUsageTracking
        {
            get
            {
                return useUsageTracking;
            }
            set
            {
                this.useUsageTracking = value;
            }
        }

    }

}