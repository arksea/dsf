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
	/// Implementation specific utilities.
	/// 
	/// @since 2.0
	/// </summary>
	internal class PoolImplUtils
	{

		/// <summary>
		/// Identifies the concrete type of object that an object factory creates.
		/// </summary>
		/// <param name="factory"> The factory to examine
		/// </param>
		/// <returns> the type of object the factory creates </returns>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @SuppressWarnings("rawtypes") static Class getFactoryType(Class factory)
		internal static java.lang.Class getFactoryType<T>(java.lang.Class factory)
		{
			return (java.lang.Class) getGenericType(typeof(PooledObjectFactory<T>), factory);
		}


		/// <summary>
		/// Obtain the concrete type used by an implementation of an interface that
		/// uses a generic type.
		/// </summary>
		/// <param name="type">  The interface that defines a generic type </param>
		/// <param name="clazz"> The class that implements the interface with a concrete type </param>
		/// @param <T>   The interface type
		/// </param>
		/// <returns> concrete type used by the implementation </returns>
		private static object getGenericType(java.lang.Class type, java.lang.Class clazz)
		{

			// Look to see if this class implements the generic interface

			// Get all the interfaces
			java.lang.reflect.Type[] interfaces = clazz.getGenericInterfaces();
			foreach (System.Type iface in interfaces)
			{
				// Only need to check interfaces that use generics
				if (iface is java.lang.reflect.ParameterizedType)
				{
                    java.lang.reflect.ParameterizedType pi = (java.lang.reflect.ParameterizedType)iface;
					// Look for the generic interface
					if (pi.getRawType() is java.lang.Class)
					{
						if (type.isAssignableFrom((java.lang.Class) pi.getRawType()))
						{
							return getTypeParameter(clazz, pi.getActualTypeArguments()[0]);
						}
					}
				}
			}

			// Interface not found on this class. Look at the superclass.
            java.lang.Class superClazz = (java.lang.Class)clazz.getSuperclass();

			object result = getGenericType(type, superClazz);
			if (result is java.lang.Class)
			{
				// Superclass implements interface and defines explicit type for
				// generic
				return result;
			}
			else if (result is int?)
			{
				// Superclass implements interface and defines unknown type for
				// generic
				// Map that unknown type to the generic types defined in this class
                java.lang.reflect.ParameterizedType superClassType = (java.lang.reflect.ParameterizedType)clazz.getGenericSuperclass();
				return getTypeParameter(clazz, superClassType.getActualTypeArguments()[(int)((int?) result)]);
			}
			else
			{
				// Error will be logged further up the call stack
				return null;
			}
		}


		/// <summary>
		/// For a generic parameter, return either the Class used or if the type
		/// is unknown, the index for the type in definition of the class
		/// </summary>
		/// <param name="clazz"> defining class </param>
		/// <param name="argType"> the type argument of interest
		/// </param>
		/// <returns> An instance of <seealso cref="Class"/> representing the type used by the
		///         type parameter or an instance of <seealso cref="Integer"/> representing
		///         the index for the type in the definition of the defining class </returns>
		private static object getTypeParameter(java.lang.Class clazz, java.lang.reflect.Type argType)
		{
			if (argType is java.lang.Class)
			{
				return argType;
			}
			else
			{
				java.lang.reflect.TypeVariable[] tvs = clazz.getTypeParameters();
				for (int i = 0; i < tvs.Length; i++)
				{
					if (tvs[i].Equals(argType))
					{
						return i;
					}
				}
				return null;
			}
		}
	}

}