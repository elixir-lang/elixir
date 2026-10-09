# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

import Kernel, except: [round: 1]

defmodule Float do
  @moduledoc """
  Functions for working with floating-point numbers.

  For mathematical operations on top of floating-points,
  see Erlang's [`:math`](`:math`) module.

  ## Kernel functions

  There are functions related to floating-point numbers on the `Kernel` module
  too. Here is a list of them:

    * `Kernel.round/1`: rounds a number to the nearest integer.
    * `Kernel.trunc/1`: returns the integer part of a number.

  ## Known issues

  There are some very well known problems with floating-point numbers
  and arithmetic due to the fact most decimal fractions cannot be
  represented by a floating-point binary and most operations are not exact,
  but operate on approximations. Those issues are not specific
  to Elixir, they are a property of floating-point representation itself.

  For example, the numbers 0.1 and 0.01 are two of them, what means the result
  of squaring 0.1 does not give 0.01 neither the closest representable. Here is
  what happens in this case:

    * The closest representable number to 0.1 is 0.1000000014
    * The closest representable number to 0.01 is 0.0099999997
    * Doing 0.1 * 0.1 should return 0.01, but because 0.1 is actually 0.1000000014,
      the result is 0.010000000000000002, and because this is not the closest
      representable number to 0.01, you'll get the wrong result for this operation

  There are also other known problems like flooring or rounding numbers. See
  `round/2` and `floor/2` for more details about them.

  To learn more about floating-point arithmetic visit:

    * [0.30000000000000004.com](https://0.30000000000000004.com/)
    * [What Every Programmer Should Know About Floating-Point Arithmetic](https://floating-point-gui.de/)

  """

  import Bitwise

  @power_of_2_to_52 4_503_599_627_370_496
  @precision_range 0..15
  @type precision_range :: 0..15

  @powers_of_5 0..15 |> Enum.map(&(5 ** &1)) |> List.to_tuple()
  @powers_of_10 0..15 |> Enum.map(&(10 ** &1)) |> List.to_tuple()

  @min_finite then(<<0xFFEFFFFFFFFFFFFF::64>>, fn <<num::float>> -> num end)
  @max_finite then(<<0x7FEFFFFFFFFFFFFF::64>>, fn <<num::float>> -> num end)

  @doc """
  Returns the maximum finite value for a float.

  ## Examples

      iex> Float.max_finite()
      1.7976931348623157e308

  """
  @spec max_finite() :: float
  def max_finite, do: @max_finite

  @doc """
  Returns the minimum finite value for a float.

  ## Examples

      iex> Float.min_finite()
      -1.7976931348623157e308

  """
  @spec min_finite() :: float
  def min_finite, do: @min_finite

  @doc """
  Computes `base` raised to power of `exponent`.

  `base` must be a float and `exponent` can be any number.
  However, if a negative base and a fractional exponent
  are given, it raises `ArithmeticError`.

  It always returns a float. See `Integer.pow/2` for
  exponentiation that returns integers.

  ## Examples

      iex> Float.pow(2.0, 0)
      1.0
      iex> Float.pow(2.0, 1)
      2.0
      iex> Float.pow(2.0, 10)
      1024.0
      iex> Float.pow(2.0, -1)
      0.5
      iex> Float.pow(2.0, -3)
      0.125

      iex> Float.pow(3.0, 1.5)
      5.196152422706632

      iex> Float.pow(-2.0, 3)
      -8.0
      iex> Float.pow(-2.0, 4)
      16.0

      iex> Float.pow(-1.0, 0.5)
      ** (ArithmeticError) bad argument in arithmetic expression

  """
  @doc since: "1.12.0"
  @spec pow(float, number) :: float
  def pow(base, exponent) when is_float(base) and is_number(exponent),
    do: :math.pow(base, exponent)

  @doc """
  Parses a binary into a float.

  If successful, returns a tuple in the form of `{float, remainder_of_binary}`;
  when the binary cannot be coerced into a valid float, the atom `:error` is
  returned.

  If the size of float exceeds the maximum size of `1.7976931348623157e+308`,
  `:error` is returned even though the textual representation itself might be
  well formed.

  If you want to convert a string-formatted float directly to a float,
  `String.to_float/1` can be used instead.

  ## Examples

      iex> Float.parse("34")
      {34.0, ""}
      iex> Float.parse("34.25")
      {34.25, ""}
      iex> Float.parse("56.5xyz")
      {56.5, "xyz"}

      iex> Float.parse(".12")
      :error
      iex> Float.parse("pi")
      :error
      iex> Float.parse("1.7976931348623159e+308")
      :error

  """
  @spec parse(binary) :: {float, binary} | :error
  def parse("-" <> binary) do
    case parse_unsigned(binary) do
      :error -> :error
      {number, remainder} -> {-number, remainder}
    end
  end

  def parse("+" <> binary) do
    parse_unsigned(binary)
  end

  def parse(binary) do
    parse_unsigned(binary)
  end

  defp parse_unsigned(<<digit, rest::binary>> = binary) when digit in ?0..?9,
    do: parse_mantissa(binary, rest, false)

  defp parse_unsigned(binary) when is_binary(binary), do: :error

  defp parse_mantissa(binary, <<digit, rest::binary>>, dot?) when digit in ?0..?9,
    do: parse_mantissa(binary, rest, dot?)

  defp parse_mantissa(binary, <<?., digit, rest::binary>>, false) when digit in ?0..?9,
    do: parse_mantissa(binary, rest, true)

  defp parse_mantissa(binary, <<exp_marker, digit, rest::binary>> = tail, dot?)
       when exp_marker in ~c"eE" and digit in ?0..?9,
       do: parse_exponent(binary, byte_size(binary) - byte_size(tail), rest, dot?)

  defp parse_mantissa(binary, <<exp_marker, sign, digit, rest::binary>> = tail, dot?)
       when exp_marker in ~c"eE" and sign in ~c"-+" and digit in ?0..?9,
       do: parse_exponent(binary, byte_size(binary) - byte_size(tail), rest, dot?)

  defp parse_mantissa(binary, rest, dot?), do: finish_mantissa(binary, rest, dot?)

  defp parse_exponent(binary, exp_pos, <<digit, rest::binary>>, dot?) when digit in ?0..?9,
    do: parse_exponent(binary, exp_pos, rest, dot?)

  defp parse_exponent(binary, exp_pos, rest, dot?),
    do: finish_exponent(binary, exp_pos, rest, dot?)

  defp finish_mantissa(binary, rest, _dot? = true) do
    {:erlang.binary_to_float(consumed(binary, rest)), rest}
  rescue
    ArgumentError -> :error
  end

  # Bare integer: * 1.0 casts to the nearest float without building a new binary,
  # and raises ArithmeticError on overflow (for example a 400-digit integer).
  defp finish_mantissa(binary, rest, _dot? = false) do
    {:erlang.binary_to_integer(consumed(binary, rest)) * 1.0, rest}
  rescue
    ArithmeticError -> :error
  end

  # binary_to_float/1 raises ArgumentError when the exponent is too big, e.g. "1.0e400".
  defp finish_exponent(binary, _exp_pos, rest, _dot? = true) do
    {:erlang.binary_to_float(consumed(binary, rest)), rest}
  rescue
    ArgumentError -> :error
  end

  # No decimal point, so ".0" is spliced in before the exponent (at exp_pos) to
  # form a valid float literal.
  defp finish_exponent(binary, exp_pos, rest, _dot? = false) do
    len = byte_size(binary) - byte_size(rest)

    literal =
      <<binary::binary-size(exp_pos), ".0", :binary.part(binary, exp_pos, len - exp_pos)::binary>>

    {:erlang.binary_to_float(literal), rest}
  rescue
    ArgumentError -> :error
  end

  defp consumed(binary, ""), do: binary
  defp consumed(binary, rest), do: :binary.part(binary, 0, byte_size(binary) - byte_size(rest))

  @doc """
  Rounds a float to the largest float less than or equal to `number`.

  `floor/2` also accepts a precision to round a floating-point value down
  to an arbitrary number of fractional digits (between 0 and 15).
  The operation is performed on the binary floating point, without a
  conversion to decimal.

  This function always returns a float. `Kernel.trunc/1` may be used instead to
  truncate the result to an integer afterwards.

  ## Known issues

  The behavior of `floor/2` for floats can be surprising. For example:

      iex> Float.floor(12.52, 2)
      12.51

  One may have expected it to floor to 12.52. This is not a bug.
  Most decimal fractions cannot be represented as a binary floating point
  and therefore the number above is internally represented as 12.51999999,
  which explains the behavior above.

  ## Examples

      iex> Float.floor(34.25)
      34.0
      iex> Float.floor(-56.5)
      -57.0
      iex> Float.floor(34.259, 2)
      34.25

  """
  @spec floor(float, precision_range) :: float
  def floor(number, precision \\ 0)

  def floor(number, 0) when is_float(number) do
    :math.floor(number)
  end

  def floor(number, precision) when is_float(number) and precision in @precision_range do
    round(number, precision, :floor)
  end

  def floor(number, precision) when is_float(number) do
    raise ArgumentError, invalid_precision_message(precision)
  end

  @doc """
  Rounds a float to the smallest float greater than or equal to `number`.

  `ceil/2` also accepts a precision to round a floating-point value up
  to an arbitrary number of fractional digits (between 0 and 15).

  The operation is performed on the binary floating point, without a
  conversion to decimal.

  The behavior of `ceil/2` for floats can be surprising. For example:

      iex> Float.ceil(-12.52, 2)
      -12.51

  One may have expected it to ceil to -12.52. This is not a bug.
  Most decimal fractions cannot be represented as a binary floating point
  and therefore the number above is internally represented as -12.51999999,
  which explains the behavior above.

  This function always returns floats. `Kernel.trunc/1` may be used instead to
  truncate the result to an integer afterwards.

  ## Examples

      iex> Float.ceil(34.25)
      35.0
      iex> Float.ceil(-56.5)
      -56.0
      iex> Float.ceil(34.251, 2)
      34.26
      iex> Float.ceil(-0.01)
      -0.0

  """
  @spec ceil(float, precision_range) :: float
  def ceil(number, precision \\ 0)

  def ceil(number, 0) when is_float(number) do
    :math.ceil(number)
  end

  def ceil(number, precision) when is_float(number) and precision in @precision_range do
    round(number, precision, :ceil)
  end

  def ceil(number, precision) when is_float(number) do
    raise ArgumentError, invalid_precision_message(precision)
  end

  @doc """
  Rounds a floating-point value to an arbitrary number of fractional
  digits (between 0 and 15).

  The rounding direction always ties to half up. The operation is
  performed on the binary floating point, without a conversion to decimal.

  This function only accepts floats and always returns a float. Use
  `Kernel.round/1` if you want a function that accepts both floats
  and integers and always returns an integer.

  ## Known issues

  The behavior of `round/2` for floats can be surprising. For example:

      iex> Float.round(5.5675, 3)
      5.567

  One may have expected it to round to the half up 5.568. This is not a bug.
  Most decimal fractions cannot be represented as a binary floating point
  and therefore the number above is internally represented as 5.567499999,
  which explains the behavior above. If you want exact rounding for decimals,
  you must use a decimal library. The behavior above is also in accordance
  with reference implementations, such as "Correctly Rounded Binary-Decimal and
  Decimal-Binary Conversions" by David M. Gay.

  ## Examples

      iex> Float.round(12.5)
      13.0
      iex> Float.round(5.5674, 3)
      5.567
      iex> Float.round(5.5675, 3)
      5.567
      iex> Float.round(-5.5674, 3)
      -5.567
      iex> Float.round(-5.5675)
      -6.0
      iex> Float.round(12.341444444444441, 15)
      12.341444444444441
      iex> Float.round(-0.01)
      -0.0

  """
  @spec round(float, precision_range) :: float
  def round(float, precision \\ 0)

  def round(float, 0) when float === 0.0 or float === -0.0, do: float

  def round(float, 0) when is_float(float) do
    case :erlang.round(float) do
      0 when float < 0.0 -> -0.0
      0 -> 0.0
      rounded -> rounded * 1.0
    end
  end

  def round(float, precision) when is_float(float) and precision in @precision_range do
    round(float, precision, :half_up)
  end

  def round(float, precision) when is_float(float) do
    raise ArgumentError, invalid_precision_message(precision)
  end

  # Round decimal places with exact integer arithmetic.
  # exp is the biased binary exponent, and 1075 = 1023 + 52.
  #
  # 1. Split the float: |float| = significand / 2^(1075 - exp).
  # 2. Scale the value: |float| * 10^precision = significand * 5^precision / 2^scaled_shift,
  #    where scaled_shift = 1075 - exp - precision.
  #    The product needs at most 88 bits: 53 for the significand and 35 for
  #    the power of five. BEAM integer arithmetic keeps the product exact.
  # 3. Round the scaled value to an integer. Use half_up, floor, or ceil.
  # 4. Convert rounded_int / 10^precision to the nearest float:
  #    - Fast path: rounded_int < 2^53. Both operands are exact as floats,
  #      so IEEE float division gives the nearest float.
  #    - Slow path: use integer division to keep two extra bits. Use these
  #      bits and the division remainder to round to nearest-even.
  #
  # Keep steps 3 and 4 separate. Step 3 selects the exact decimal value.
  # Step 4 selects the nearest float. This prevents double-rounding errors.
  defp round(num, _precision, _rounding) when is_float(num) and num == 0.0, do: num

  defp round(float, precision, mode) do
    <<sign::1, exp::11, mantissa::52>> = <<float::float>>

    cond do
      # exp <= 971 means |float| < 2^-51, so |float * 10^precision| < 0.5.
      # Half-up gives zero. Floor and ceil also depend on the sign.
      exp <= 971 ->
        tiny_round(sign, precision, mode)

      # The binary denominator divides 10^precision, so decimal rounding changes nothing.
      exp >= 1075 - precision ->
        float

      true ->
        significand = @power_of_2_to_52 ||| mantissa
        scaled_shift = 1075 - precision - exp
        product = significand * elem(@powers_of_5, precision)

        # Let x = product / 2^scaled_shift.
        # Half-up uses floor(x + 1/2) == floor((floor(2x) + 1) / 2).
        # Floor of a negative value and ceil of a positive value increase
        # the magnitude. Use ceil(x) == -floor(-x) for these cases.
        rounded_int =
          case mode do
            :half_up ->
              ((product >>> (scaled_shift - 1)) + 1) >>> 1

            :floor when sign == 1 ->
              -(-product >>> scaled_shift)

            :ceil when sign == 0 ->
              -(-product >>> scaled_shift)

            _ ->
              product >>> scaled_shift
          end

        decimal_scale = elem(@powers_of_10, precision)

        cond do
          rounded_int == 0 ->
            signed_zero(sign)

          rounded_int < @power_of_2_to_52 <<< 1 ->
            # Both operands fit in 53 bits, so IEEE float division gives the
            # nearest float. Apply the sign before BEAM allocates the float.
            if sign == 0 do
              rounded_int / decimal_scale
            else
              -(rounded_int / decimal_scale)
            end

          true ->
            bignum_to_float(sign, rounded_int, decimal_scale, exp)
        end
    end
  end

  @compile {:inline, signed_zero: 1, tiny_round: 3}
  defp signed_zero(0), do: 0.0
  defp signed_zero(1), do: -0.0

  # Round a nonzero float with |float * 10^precision| < 0.5.
  # Ceil of a positive value gives 10^-precision.
  # Floor of a negative value gives -10^-precision.
  # All other cases give zero with the input sign.
  defp tiny_round(0, precision, :ceil), do: 1.0 / elem(@powers_of_10, precision)
  defp tiny_round(1, precision, :floor), do: -1.0 / elem(@powers_of_10, precision)
  defp tiny_round(sign, _precision, _mode), do: signed_zero(sign)

  # Return the nearest float to rounded_int / decimal_scale. Apply the sign.
  # This path requires rounded_int >= 2^53.
  # Use nearest-even for binary rounding, for all decimal rounding modes.
  defp bignum_to_float(sign, rounded_int, decimal_scale, exp) do
    # rounded_int >= 2^53 and decimal_scale <= 10^15, so the input magnitude
    # is greater than 9. The limits of its binary exponent interval are
    # integers. These limits are exact at every allowed decimal precision.
    # Decimal rounding keeps the value inside these limits.
    # The rounded value can equal the upper limit.
    #
    # Scale with the input exponent to keep two guard bits.
    # quotient is in [2^54, 2^55], and significand is in [2^52, 2^53].
    # At precision 1, numerator stays below 2^59 and fits in a small integer
    # on a 64-bit BEAM.
    numerator = rounded_int <<< (1077 - exp)
    quotient = div(numerator, decimal_scale)
    significand = quotient >>> 2

    guard_bits = quotient &&& 3

    # When the guard bits equal 2, round up if the significand is odd or
    # the division has a remainder.
    rounded_significand =
      cond do
        guard_bits > 2 -> significand + 1
        guard_bits < 2 -> significand
        (significand &&& 1) == 1 -> significand + 1
        rem(numerator, decimal_scale) != 0 -> significand + 1
        true -> significand
      end

    # Both the upper limit and a rounding carry give 2^53. Its low 52 bits
    # give a zero fraction. Its high bit increases the exponent by one.
    biased_exp = exp + (rounded_significand >>> 53)
    <<result::float>> = <<sign::1, biased_exp::11, rounded_significand::52>>
    result
  end

  @doc """
  Returns a pair of integers whose ratio is exactly equal
  to the original float and with a positive denominator.

  ## Examples

      iex> Float.ratio(0.0)
      {0, 1}
      iex> Float.ratio(3.14)
      {7070651414971679, 2251799813685248}
      iex> Float.ratio(-3.14)
      {-7070651414971679, 2251799813685248}
      iex> Float.ratio(1.5)
      {3, 2}
      iex> Float.ratio(-1.5)
      {-3, 2}
      iex> Float.ratio(16.0)
      {16, 1}
      iex> Float.ratio(-16.0)
      {-16, 1}

  """
  @doc since: "1.4.0"
  @spec ratio(float) :: {integer, pos_integer}
  def ratio(float) when is_float(float) and float == 0.0, do: {0, 1}

  def ratio(float) when is_float(float) do
    <<sign::1, exp::11, mantissa::52>> = <<float::float>>

    if exp != 0 do
      if mantissa == 0 do
        to_ratio(1, exp - 1023, sign)
      else
        # Normal float magnitudes are (2^52 + mantissa) * 2^(exp - 1075).
        reduce_to_ratio(mantissa ||| @power_of_2_to_52, exp - 1075, sign)
      end
    else
      # Subnormal float magnitudes are mantissa * 2^-1074.
      reduce_to_ratio(mantissa, -1074, sign)
    end
  end

  # Strip trailing zero bits in chunks to reduce recursive calls.
  defp reduce_to_ratio(significand, exp2, sign) when (significand &&& 1) == 1,
    do: to_ratio(significand, exp2, sign)

  defp reduce_to_ratio(significand, exp2, sign) when (significand &&& 0xFFFF) == 0,
    do: reduce_to_ratio(significand >>> 16, exp2 + 16, sign)

  defp reduce_to_ratio(significand, exp2, sign) when (significand &&& 0xF) == 0,
    do: reduce_to_ratio(significand >>> 4, exp2 + 4, sign)

  defp reduce_to_ratio(significand, exp2, sign),
    do: reduce_to_ratio(significand >>> 1, exp2 + 1, sign)

  @compile {:inline, to_ratio: 3}
  defp to_ratio(significand, exp2, sign) do
    significand = if sign == 1, do: -significand, else: significand

    if exp2 >= 0 do
      {significand <<< exp2, 1}
    else
      {significand, 1 <<< -exp2}
    end
  end

  @doc """
  Returns a charlist which corresponds to the shortest text representation
  of the given float.

  It uses the algorithm presented in "Ryū: fast float-to-string conversion"
  in Proceedings of the SIGPLAN '2018 Conference on Programming Language
  Design and Implementation.

  For a configurable representation, use `:erlang.float_to_list/2`.

  Inlined by the compiler.

  ## Examples

      iex> Float.to_charlist(7.0)
      ~c"7.0"

  """
  @spec to_charlist(float) :: charlist
  def to_charlist(float) do
    :erlang.float_to_list(float, [:short])
  end

  @doc """
  Returns a binary which corresponds to the shortest text representation
  of the given float.

  The underlying algorithm changes depending on the Erlang/OTP version:

    * For OTP >= 24, it uses the algorithm presented in "Ryū: fast
      float-to-string conversion" in Proceedings of the SIGPLAN '2018
      Conference on Programming Language Design and Implementation.

    * For OTP < 24, it uses the algorithm presented in "Printing Floating-Point
      Numbers Quickly and Accurately" in Proceedings of the SIGPLAN '1996
      Conference on Programming Language Design and Implementation.

  For a configurable representation, use `:erlang.float_to_binary/2`.

  Inlined by the compiler.

  ## Examples

      iex> Float.to_string(7.0)
      "7.0"

  """
  @spec to_string(float) :: String.t()
  def to_string(float) do
    :erlang.float_to_binary(float, [:short])
  end

  @doc false
  @deprecated "Use Float.to_charlist/1 instead"
  def to_char_list(float), do: Float.to_charlist(float)

  @doc false
  @deprecated "Use :erlang.float_to_list/2 instead"
  def to_char_list(float, options) do
    :erlang.float_to_list(float, expand_compact(options))
  end

  @doc false
  @deprecated "Use :erlang.float_to_binary/2 instead"
  def to_string(float, options) do
    :erlang.float_to_binary(float, expand_compact(options))
  end

  defp invalid_precision_message(precision) do
    "precision #{inspect(precision)} is out of valid range of #{inspect(@precision_range)}"
  end

  defp expand_compact([{:compact, false} | t]), do: expand_compact(t)
  defp expand_compact([{:compact, true} | t]), do: [:compact | expand_compact(t)]
  defp expand_compact([h | t]), do: [h | expand_compact(t)]
  defp expand_compact([]), do: []
end
