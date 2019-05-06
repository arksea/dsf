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
	/// Implementation of object that is used to provide information on pooled
	/// objects via JMX.
	/// 
	/// @since 2.0
	/// </summary>
	public class DefaultPooledObjectInfo<T> : DefaultPooledObjectInfoMBean
	{
		private readonly PooledObject<T> pooledObject;

		/// <summary>
		/// Create a new instance for the given pooled object.
		/// </summary>
		/// <param name="pooledObject"> The pooled object that this instance will represent </param>
		public DefaultPooledObjectInfo(PooledObject<T> pooledObject)
		{
			this.pooledObject = pooledObject;
		}

		public virtual long CreateTime
		{
			get
			{
				return pooledObject.CreateTime;
			}
		}

		public virtual string CreateTimeFormatted
		{
			get
			{

                java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
				return sdf.format(pooledObject.CreateTime);
			}
		}

		public virtual long LastBorrowTime
		{
			get
			{
				return pooledObject.LastBorrowTime;
			}
		}

		public virtual string LastBorrowTimeFormatted
		{
			get
			{
                java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
				return sdf.format(pooledObject.LastBorrowTime);
			}
		}

		public virtual string LastBorrowTrace
		{
			get
			{
				java.io.StringWriter sw = new java.io.StringWriter();
				pooledObject.printStackTrace(new java.io.PrintWriter(sw));
				return sw.ToString();
			}
		}

		public virtual long LastReturnTime
		{
			get
			{
				return pooledObject.LastReturnTime;
			}
		}

		public virtual string LastReturnTimeFormatted
		{
			get
			{
                java.text.SimpleDateFormat sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z");
				return sdf.format(pooledObject.LastReturnTime);
			}
		}

		public virtual string PooledObjectType
		{
			get
			{
	//JAVA TO C# CONVERTER WARNING: The .NET Type.FullName property will not always yield results identical to the Java Class.getName method:
				return pooledObject.Object.GetType().FullName;
			}
		}

		public virtual string PooledObjectToString
		{
			get
			{
				return pooledObject.Object.ToString();
			}
		}

		public virtual long BorrowedCount
		{
			get
			{
				// TODO Simplify this once getBorrowedCount has been added to PooledObject
				if (pooledObject is DefaultPooledObject<T>)
				{
					return ((DefaultPooledObject<T>) pooledObject).BorrowedCount;
				}
				else
				{
					return -1;
				}
			}
		}
	}

}