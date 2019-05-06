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
    /// A simple "struct" encapsulating the configuration for a
    /// <seealso cref="GenericObjectPool"/>.
    /// 
    /// <para>
    /// This class is not thread-safe; it is only intended to be used to provide
    /// attributes used when creating a pool.
    /// 
    /// @version $Revision: $
    /// 
    /// @since 2.0
    /// </para>
    /// </summary>
    public class GenericObjectPoolConfig : BaseObjectPoolConfig
    {

        /// <summary>
        /// The default value for the {@code maxTotal} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getMaxTotal() </seealso>
        public const int DEFAULT_MAX_TOTAL = 8;

        /// <summary>
        /// The default value for the {@code maxIdle} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getMaxIdle() </seealso>
        public const int DEFAULT_MAX_IDLE = 8;

        /// <summary>
        /// The default value for the {@code minIdle} configuration attribute. </summary>
        /// <seealso cref= GenericObjectPool#getMinIdle() </seealso>
        public const int DEFAULT_MIN_IDLE = 0;


        private int maxTotal = DEFAULT_MAX_TOTAL;

        private int maxIdle = DEFAULT_MAX_IDLE;

        private int minIdle = DEFAULT_MIN_IDLE;

        /// <summary>
        /// Get the value for the {@code maxTotal} configuration attribute
        /// for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code maxTotal} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getMaxTotal() </seealso>
        public virtual int MaxTotal
        {
            get
            {
                return maxTotal;
            }
            set
            {
                this.maxTotal = value;
            }
        }



        /// <summary>
        /// Get the value for the {@code maxIdle} configuration attribute
        /// for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code maxIdle} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getMaxIdle() </seealso>
        public virtual int MaxIdle
        {
            get
            {
                return maxIdle;
            }
            set
            {
                this.maxIdle = value;
            }
        }



        /// <summary>
        /// Get the value for the {@code minIdle} configuration attribute
        /// for pools created with this configuration instance.
        /// </summary>
        /// <returns>  The current setting of {@code minIdle} for this
        ///          configuration instance
        /// </returns>
        /// <seealso cref= GenericObjectPool#getMinIdle() </seealso>
        public virtual int MinIdle
        {
            get
            {
                return minIdle;
            }
            set
            {
                this.minIdle = value;
            }
        }


        public override object Clone()
        {
            return base.MemberwiseClone();
        }
    }

}