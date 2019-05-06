using System;

/*
 * Copyright (C) 2009 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace com.google.common.@base
{

	/// <summary>
	/// Methods factored out so that they can be emulated differently in GWT.
	/// 
	/// @author Jesse Wilson
	/// </summary>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @GwtCompatible(emulated = true) final class Platform
	internal sealed class Platform
	{
	  private Platform()
	  {
	  }

      private static readonly System.DateTime Jan1st1970 = new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
	  /// <summary>
	  /// Calls <seealso cref="System#nanoTime()"/>. </summary>
	  internal static long systemNanoTime()
	  {
        return (long)(System.DateTime.UtcNow - Jan1st1970).Ticks*100;
	  }
      /*
	  internal static CharMatcher precomputeCharMatcher(CharMatcher matcher)
	  {
		return matcher.precomputedInternal();
	  }

	  internal static Optional<T> getEnumIfPresent<T>(Type enumClass, string value) where T : Enum<T>
	  {
//JAVA TO C# CONVERTER TODO TASK: Java wildcard generics are not converted to .NET:
//ORIGINAL LINE: WeakReference<? extends Enum<?>> ref = Enums.getEnumConstants(enumClass).get(value);
		WeakReference<?> @ref = Enums.getEnumConstants(enumClass).get(value);
		return @ref == null ? Optional.absent<T>() : Optional.of(enumClass.cast(@ref.get()));
	  }
      */
	}

}