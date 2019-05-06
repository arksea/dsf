using System;
using System.Text;

/*
 * Copyright (C) 2007 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

namespace com.google.common.@base
{

	/// <summary>
	/// Static convenience methods that help a method or constructor check whether it was invoked
	/// correctly (whether its <i>preconditions</i> have been met). These methods generally accept a
	/// {@code boolean} expression which is expected to be {@code true} (or in the case of {@code
	/// checkNotNull}, an object reference which is expected to be non-null). When {@code false} (or
	/// {@code null}) is passed instead, the {@code Preconditions} method throws an unchecked exception,
	/// which helps the calling method communicate to <i>its</i> caller that <i>that</i> caller has made
	/// a mistake. Example: <pre>   {@code
	/// 
	///   /**
	///    * Returns the positive square root of the given value.
	///    * </summary>
	///    * <exception cref="IllegalArgumentException"> if the value is negative
	///    *}{@code /
	///   public static double sqrt(double value) {
	///     Preconditions.checkArgument(value >= 0.0, "negative value: %s", value);
	///     // calculate the square root
	///   }
	/// 
	///   void exampleBadCaller() {
	///     double d = sqrt(-1.0);
	///   }}</pre>
	/// 
	/// In this example, {@code checkArgument} throws an {@code IllegalArgumentException} to indicate
	/// that {@code exampleBadCaller} made an error in <i>its</i> call to {@code sqrt}.
	/// 
	/// <h3>Warning about performance</h3>
	/// 
	/// <para>The goal of this class is to improve readability of code, but in some circumstances this may
	/// come at a significant performance cost. Remember that parameter values for message construction
	/// must all be computed eagerly, and autoboxing and varargs array creation may happen as well, even
	/// when the precondition check then succeeds (as it should almost always do in production). In some
	/// circumstances these wasted CPU cycles and allocations can add up to a real problem.
	/// Performance-sensitive precondition checks can always be converted to the customary form:
	/// <pre>   {@code
	/// 
	///   if (value < 0.0) {
	///     throw new IllegalArgumentException("negative value: " + value);
	///   }}</pre>
	/// 
	/// <h3>Other types of preconditions</h3>
	/// 
	/// </para>
	/// <para>Not every type of precondition failure is supported by these methods. Continue to throw
	/// standard JDK exceptions such as <seealso cref="java.util.NoSuchElementException"/> or {@link
	/// UnsupportedOperationException} in the situations they are intended for.
	/// 
	/// <h3>Non-preconditions</h3>
	/// 
	/// </para>
	/// <para>It is of course possible to use the methods of this class to check for invalid conditions
	/// which are <i>not the caller's fault</i>. Doing so is <b>not recommended</b> because it is
	/// misleading to future readers of the code and of stack traces. See
	/// <a href="http://code.google.com/p/guava-libraries/wiki/ConditionalFailuresExplained">Conditional
	/// failures explained</a> in the Guava User Guide for more advice.
	/// 
	/// <h3>{@code java.util.Objects.requireNonNull()}</h3>
	/// 
	/// </para>
	/// <para>Projects which use {@code com.google.common} should generally avoid the use of {@link
	/// java.util.Objects#requireNonNull(Object)}. Instead, use whichever of {@link
	/// #checkNotNull(Object)} or <seealso cref="Verify#verifyNotNull(Object)"/> is appropriate to the situation.
	/// (The same goes for the message-accepting overloads.)
	/// 
	/// <h3>Only {@code %s} is supported</h3>
	/// 
	/// </para>
	/// <para>In {@code Preconditions} error message template strings, only the {@code "%s"} specifier is
	/// supported, not the full range of <seealso cref="java.util.Formatter"/> specifiers.
	/// 
	/// <h3>More information</h3>
	/// 
	/// </para>
	/// <para>See the Guava User Guide on
	/// <a href="http://code.google.com/p/guava-libraries/wiki/PreconditionsExplained">using {@code
	/// Preconditions}</a>.
	/// 
	/// @author Kevin Bourrillion
	/// @since 2.0 (imported from Google Collections Library) </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: @GwtCompatible public final class Preconditions
	public sealed class Preconditions
	{
	  private Preconditions()
	  {
	  }

	  /// <summary>
	  /// Ensures the truth of an expression involving one or more parameters to the calling method.
	  /// </summary>
	  /// <param name="expression"> a boolean expression </param>
	  /// <exception cref="IllegalArgumentException"> if {@code expression} is false </exception>
	  public static void checkArgument(bool expression)
	  {
		if (!expression)
		{
		  throw new System.ArgumentException();
		}
	  }

	  /// <summary>
	  /// Ensures the truth of an expression involving one or more parameters to the calling method.
	  /// </summary>
	  /// <param name="expression"> a boolean expression </param>
	  /// <param name="errorMessage"> the exception message to use if the check fails; will be converted to a
	  ///     string using <seealso cref="String#valueOf(Object)"/> </param>
	  /// <exception cref="IllegalArgumentException"> if {@code expression} is false </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static void checkArgument(boolean expression, @Nullable Object errorMessage)
	  public static void checkArgument(bool expression, object errorMessage)
	  {
		if (!expression)
		{
		  throw new System.ArgumentException(Convert.ToString(errorMessage));
		}
	  }

	  /// <summary>
	  /// Ensures the truth of an expression involving one or more parameters to the calling method.
	  /// </summary>
	  /// <param name="expression"> a boolean expression </param>
	  /// <param name="errorMessageTemplate"> a template for the exception message should the check fail. The
	  ///     message is formed by replacing each {@code %s} placeholder in the template with an
	  ///     argument. These are matched by position - the first {@code %s} gets {@code
	  ///     errorMessageArgs[0]}, etc.  Unmatched arguments will be appended to the formatted message
	  ///     in square braces. Unmatched placeholders will be left as-is. </param>
	  /// <param name="errorMessageArgs"> the arguments to be substituted into the message template. Arguments
	  ///     are converted to strings using <seealso cref="String#valueOf(Object)"/>. </param>
	  /// <exception cref="IllegalArgumentException"> if {@code expression} is false </exception>
	  /// <exception cref="NullPointerException"> if the check fails and either {@code errorMessageTemplate} or
	  ///     {@code errorMessageArgs} is null (don't let this happen) </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static void checkArgument(boolean expression, @Nullable String errorMessageTemplate, @Nullable Object... errorMessageArgs)
	  public static void checkArgument(bool expression, string errorMessageTemplate, params object[] errorMessageArgs)
	  {
		if (!expression)
		{
		  throw new System.ArgumentException(format(errorMessageTemplate, errorMessageArgs));
		}
	  }

	  /// <summary>
	  /// Ensures the truth of an expression involving the state of the calling instance, but not
	  /// involving any parameters to the calling method.
	  /// </summary>
	  /// <param name="expression"> a boolean expression </param>
	  /// <exception cref="IllegalStateException"> if {@code expression} is false </exception>
	  public static void checkState(bool expression)
	  {
		if (!expression)
		{
            throw new ApplicationException();
		}
	  }

	  /// <summary>
	  /// Ensures the truth of an expression involving the state of the calling instance, but not
	  /// involving any parameters to the calling method.
	  /// </summary>
	  /// <param name="expression"> a boolean expression </param>
	  /// <param name="errorMessage"> the exception message to use if the check fails; will be converted to a
	  ///     string using <seealso cref="String#valueOf(Object)"/> </param>
	  /// <exception cref="IllegalStateException"> if {@code expression} is false </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static void checkState(boolean expression, @Nullable Object errorMessage)
	  public static void checkState(bool expression, object errorMessage)
	  {
		if (!expression)
		{
            throw new ApplicationException(Convert.ToString(errorMessage));
		}
	  }

	  /// <summary>
	  /// Ensures the truth of an expression involving the state of the calling instance, but not
	  /// involving any parameters to the calling method.
	  /// </summary>
	  /// <param name="expression"> a boolean expression </param>
	  /// <param name="errorMessageTemplate"> a template for the exception message should the check fail. The
	  ///     message is formed by replacing each {@code %s} placeholder in the template with an
	  ///     argument. These are matched by position - the first {@code %s} gets {@code
	  ///     errorMessageArgs[0]}, etc.  Unmatched arguments will be appended to the formatted message
	  ///     in square braces. Unmatched placeholders will be left as-is. </param>
	  /// <param name="errorMessageArgs"> the arguments to be substituted into the message template. Arguments
	  ///     are converted to strings using <seealso cref="String#valueOf(Object)"/>. </param>
	  /// <exception cref="IllegalStateException"> if {@code expression} is false </exception>
	  /// <exception cref="NullPointerException"> if the check fails and either {@code errorMessageTemplate} or
	  ///     {@code errorMessageArgs} is null (don't let this happen) </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static void checkState(boolean expression, @Nullable String errorMessageTemplate, @Nullable Object... errorMessageArgs)
	  public static void checkState(bool expression, string errorMessageTemplate, params object[] errorMessageArgs)
	  {
		if (!expression)
		{
		  throw new ApplicationException(format(errorMessageTemplate, errorMessageArgs));
		}
	  }

	  /// <summary>
	  /// Ensures that an object reference passed as a parameter to the calling method is not null.
	  /// </summary>
	  /// <param name="reference"> an object reference </param>
	  /// <returns> the non-null reference that was validated </returns>
	  /// <exception cref="NullPointerException"> if {@code reference} is null </exception>
	  public static T checkNotNull<T>(T reference)
	  {
		if (reference == null)
		{
		  throw new System.NullReferenceException();
		}
		return reference;
	  }

	  /// <summary>
	  /// Ensures that an object reference passed as a parameter to the calling method is not null.
	  /// </summary>
	  /// <param name="reference"> an object reference </param>
	  /// <param name="errorMessage"> the exception message to use if the check fails; will be converted to a
	  ///     string using <seealso cref="String#valueOf(Object)"/> </param>
	  /// <returns> the non-null reference that was validated </returns>
	  /// <exception cref="NullPointerException"> if {@code reference} is null </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static <T> T checkNotNull(T reference, @Nullable Object errorMessage)
	  public static T checkNotNull<T>(T reference, object errorMessage)
	  {
		if (reference == null)
		{
		  throw new System.NullReferenceException(Convert.ToString(errorMessage));
		}
		return reference;
	  }

	  /// <summary>
	  /// Ensures that an object reference passed as a parameter to the calling method is not null.
	  /// </summary>
	  /// <param name="reference"> an object reference </param>
	  /// <param name="errorMessageTemplate"> a template for the exception message should the check fail. The
	  ///     message is formed by replacing each {@code %s} placeholder in the template with an
	  ///     argument. These are matched by position - the first {@code %s} gets {@code
	  ///     errorMessageArgs[0]}, etc.  Unmatched arguments will be appended to the formatted message
	  ///     in square braces. Unmatched placeholders will be left as-is. </param>
	  /// <param name="errorMessageArgs"> the arguments to be substituted into the message template. Arguments
	  ///     are converted to strings using <seealso cref="String#valueOf(Object)"/>. </param>
	  /// <returns> the non-null reference that was validated </returns>
	  /// <exception cref="NullPointerException"> if {@code reference} is null </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static <T> T checkNotNull(T reference, @Nullable String errorMessageTemplate, @Nullable Object... errorMessageArgs)
	  public static T checkNotNull<T>(T reference, string errorMessageTemplate, params object[] errorMessageArgs)
	  {
		if (reference == null)
		{
		  // If either of these parameters is null, the right thing happens anyway
		  throw new System.NullReferenceException(format(errorMessageTemplate, errorMessageArgs));
		}
		return reference;
	  }

	  /*
	   * All recent hotspots (as of 2009) *really* like to have the natural code
	   *
	   * if (guardExpression) {
	   *    throw new BadException(messageExpression);
	   * }
	   *
	   * refactored so that messageExpression is moved to a separate String-returning method.
	   *
	   * if (guardExpression) {
	   *    throw new BadException(badMsg(...));
	   * }
	   *
	   * The alternative natural refactorings into void or Exception-returning methods are much slower.
	   * This is a big deal - we're talking factors of 2-8 in microbenchmarks, not just 10-20%.  (This
	   * is a hotspot optimizer bug, which should be fixed, but that's a separate, big project).
	   *
	   * The coding pattern above is heavily used in java.util, e.g. in ArrayList.  There is a
	   * RangeCheckMicroBenchmark in the JDK that was used to test this.
	   *
	   * But the methods in this class want to throw different exceptions, depending on the args, so it
	   * appears that this pattern is not directly applicable.  But we can use the ridiculous, devious
	   * trick of throwing an exception in the middle of the construction of another exception.  Hotspot
	   * is fine with that.
	   */

	  /// <summary>
	  /// Ensures that {@code index} specifies a valid <i>element</i> in an array, list or string of size
	  /// {@code size}. An element index may range from zero, inclusive, to {@code size}, exclusive.
	  /// </summary>
	  /// <param name="index"> a user-supplied index identifying an element of an array, list or string </param>
	  /// <param name="size"> the size of that array, list or string </param>
	  /// <returns> the value of {@code index} </returns>
	  /// <exception cref="IndexOutOfBoundsException"> if {@code index} is negative or is not less than {@code size} </exception>
	  /// <exception cref="IllegalArgumentException"> if {@code size} is negative </exception>
	  public static int checkElementIndex(int index, int size)
	  {
		return checkElementIndex(index, size, "index");
	  }

	  /// <summary>
	  /// Ensures that {@code index} specifies a valid <i>element</i> in an array, list or string of size
	  /// {@code size}. An element index may range from zero, inclusive, to {@code size}, exclusive.
	  /// </summary>
	  /// <param name="index"> a user-supplied index identifying an element of an array, list or string </param>
	  /// <param name="size"> the size of that array, list or string </param>
	  /// <param name="desc"> the text to use to describe this index in an error message </param>
	  /// <returns> the value of {@code index} </returns>
	  /// <exception cref="IndexOutOfBoundsException"> if {@code index} is negative or is not less than {@code size} </exception>
	  /// <exception cref="IllegalArgumentException"> if {@code size} is negative </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static int checkElementIndex(int index, int size, @Nullable String desc)
	  public static int checkElementIndex(int index, int size, string desc)
	  {
		// Carefully optimized for execution by hotspot (explanatory comment above)
		if (index < 0 || index >= size)
		{
		  throw new System.IndexOutOfRangeException(badElementIndex(index, size, desc));
		}
		return index;
	  }

	  private static string badElementIndex(int index, int size, string desc)
	  {
		if (index < 0)
		{
		  return format("%s (%s) must not be negative", desc, index);
		}
		else if (size < 0)
		{
		  throw new System.ArgumentException("negative size: " + size);
		} // index >= size
		else
		{
		  return format("%s (%s) must be less than size (%s)", desc, index, size);
		}
	  }

	  /// <summary>
	  /// Ensures that {@code index} specifies a valid <i>position</i> in an array, list or string of
	  /// size {@code size}. A position index may range from zero to {@code size}, inclusive.
	  /// </summary>
	  /// <param name="index"> a user-supplied index identifying a position in an array, list or string </param>
	  /// <param name="size"> the size of that array, list or string </param>
	  /// <returns> the value of {@code index} </returns>
	  /// <exception cref="IndexOutOfBoundsException"> if {@code index} is negative or is greater than {@code size} </exception>
	  /// <exception cref="IllegalArgumentException"> if {@code size} is negative </exception>
	  public static int checkPositionIndex(int index, int size)
	  {
		return checkPositionIndex(index, size, "index");
	  }

	  /// <summary>
	  /// Ensures that {@code index} specifies a valid <i>position</i> in an array, list or string of
	  /// size {@code size}. A position index may range from zero to {@code size}, inclusive.
	  /// </summary>
	  /// <param name="index"> a user-supplied index identifying a position in an array, list or string </param>
	  /// <param name="size"> the size of that array, list or string </param>
	  /// <param name="desc"> the text to use to describe this index in an error message </param>
	  /// <returns> the value of {@code index} </returns>
	  /// <exception cref="IndexOutOfBoundsException"> if {@code index} is negative or is greater than {@code size} </exception>
	  /// <exception cref="IllegalArgumentException"> if {@code size} is negative </exception>
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: public static int checkPositionIndex(int index, int size, @Nullable String desc)
	  public static int checkPositionIndex(int index, int size, string desc)
	  {
		// Carefully optimized for execution by hotspot (explanatory comment above)
		if (index < 0 || index > size)
		{
		  throw new System.IndexOutOfRangeException(badPositionIndex(index, size, desc));
		}
		return index;
	  }

	  private static string badPositionIndex(int index, int size, string desc)
	  {
		if (index < 0)
		{
		  return format("%s (%s) must not be negative", desc, index);
		}
		else if (size < 0)
		{
		  throw new System.ArgumentException("negative size: " + size);
		} // index > size
		else
		{
		  return format("%s (%s) must not be greater than size (%s)", desc, index, size);
		}
	  }

	  /// <summary>
	  /// Ensures that {@code start} and {@code end} specify a valid <i>positions</i> in an array, list
	  /// or string of size {@code size}, and are in order. A position index may range from zero to
	  /// {@code size}, inclusive.
	  /// </summary>
	  /// <param name="start"> a user-supplied index identifying a starting position in an array, list or string </param>
	  /// <param name="end"> a user-supplied index identifying a ending position in an array, list or string </param>
	  /// <param name="size"> the size of that array, list or string </param>
	  /// <exception cref="IndexOutOfBoundsException"> if either index is negative or is greater than {@code size},
	  ///     or if {@code end} is less than {@code start} </exception>
	  /// <exception cref="IllegalArgumentException"> if {@code size} is negative </exception>
	  public static void checkPositionIndexes(int start, int end, int size)
	  {
		// Carefully optimized for execution by hotspot (explanatory comment above)
		if (start < 0 || end < start || end > size)
		{
		  throw new System.IndexOutOfRangeException(badPositionIndexes(start, end, size));
		}
	  }

	  private static string badPositionIndexes(int start, int end, int size)
	  {
		if (start < 0 || start > size)
		{
		  return badPositionIndex(start, size, "start index");
		}
		if (end < 0 || end > size)
		{
		  return badPositionIndex(end, size, "end index");
		}
		// end < start
		return format("end index (%s) must not be less than start index (%s)", end, start);
	  }

	  /// <summary>
	  /// Substitutes each {@code %s} in {@code template} with an argument. These are matched by
	  /// position: the first {@code %s} gets {@code args[0]}, etc.  If there are more arguments than
	  /// placeholders, the unmatched arguments will be appended to the end of the formatted message in
	  /// square braces.
	  /// </summary>
	  /// <param name="template"> a non-null string containing 0 or more {@code %s} placeholders. </param>
	  /// <param name="args"> the arguments to be substituted into the message template. Arguments are converted
	  ///     to strings using <seealso cref="String#valueOf(Object)"/>. Arguments can be null. </param>
	  // Note that this is somewhat-improperly used from Verify.java as well.
//JAVA TO C# CONVERTER TODO TASK: Most Java annotations will not have direct .NET equivalent attributes:
//ORIGINAL LINE: static String format(String template, @Nullable Object... args)
	  internal static string format(string template, params object[] args)
	  {
		template = Convert.ToString(template); // null -> "null"

		// start substituting the arguments into the '%s' placeholders
		StringBuilder builder = new StringBuilder(template.Length + 16 * args.Length);
		int templateStart = 0;
		int i = 0;
		while (i < args.Length)
		{
		  int placeholderStart = template.IndexOf("%s", templateStart, StringComparison.Ordinal);
		  if (placeholderStart == -1)
		  {
			break;
		  }
		  builder.Append(template.Substring(templateStart, placeholderStart - templateStart));
		  builder.Append(args[i++]);
		  templateStart = placeholderStart + 2;
		}
		builder.Append(template.Substring(templateStart));

		// if we run out of placeholders, append the extra args in square braces
		if (i < args.Length)
		{
		  builder.Append(" [");
		  builder.Append(args[i++]);
		  while (i < args.Length)
		  {
			builder.Append(", ");
			builder.Append(args[i++]);
		  }
		  builder.Append(']');
		}

		return builder.ToString();
	  }
	}

}