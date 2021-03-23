defmodule Bitwise do
  @moduledoc """
  A set of functions that perform calculations on bits.

  All bitwise functions work only on integers; otherwise an
  `ArithmeticError` is raised.

  The functions in this module come in two flavors: named or
  operators. For example:

      iex> use Bitwise
      iex> bnot(1) # named
      -2
      iex> 1 &&& 1 # operator
      1

  If you prefer to use only operators or skip them, you can
  pass the following options:

    * `:only_operators` - includes only operators
    * `:skip_operators` - skips operators

  For example:

      iex> use Bitwise, only_operators: true
      iex> 1 &&& 1
      1

  When invoked with no options, `use Bitwise` is equivalent
  to `import Bitwise`.

  All bitwise functions can be used in guards:

      iex> odd? = fn
      ...>   int when Bitwise.band(int, 1) == 1 -> true
      ...>   _ -> false
      ...> end
      iex> odd?.(1)
      true

  All functions in this module are inlined by the compiler.
  """

  @doc false
  defmacro __using__(options) do
    except =
      cond do
        Keyword.get(options, :only_operators) ->
          [bnot: 1, band: 2, bor: 2, bxor: 2, bsl: 2, bsr: 2]

        Keyword.get(options, :skip_operators) ->
          [~~~: 1, &&&: 2, |||: 2, ^^^: 2, <<<: 2, >>>: 2]

        true ->
          []
      end

    quote do
      import Bitwise, except: unquote(except)
    end
  end

  @doc """
  Calculates the bitwise NOT of the argument.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bnot(2)
      -3

      iex> bnot(2) &&& 3
      1

  """
  @doc guard: true
  @spec bnot(integer) :: integer
  def bnot(expr) do
    :erlang.bnot(expr)
  end

  @doc """
  Bitwise NOT unary operator.

  Calculates the bitwise NOT of the argument.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> ~~~2
      -3

      iex> ~~~2 &&& 3
      1

  """
  @doc guard: true
  @spec ~~~integer :: integer
  def ~~~expr do
    :erlang.bnot(expr)
  end

  @doc """
  Calculates the bitwise AND of its arguments.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> band(9, 3)
      1

  """
  @doc guard: true
  @spec band(integer, integer) :: integer
  def band(left, right) do
    :erlang.band(left, right)
  end

  @doc """
  Bitwise AND operator.

  Calculates the bitwise AND of its arguments.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 9 &&& 3
      1

  """
  @doc guard: true
  @spec integer &&& integer :: integer
  def left &&& right do
    :erlang.band(left, right)
  end

  @doc """
  Calculates the bitwise OR of its arguments.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bor(9, 3)
      11

  """
  @doc guard: true
  @spec bor(integer, integer) :: integer
  def bor(left, right) do
    :erlang.bor(left, right)
  end

  @doc """
  Bitwise OR operator.

  Calculates the bitwise OR of its arguments.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 9 ||| 3
      11

  """
  @doc guard: true
  @spec integer ||| integer :: integer
  def left ||| right do
    :erlang.bor(left, right)
  end

  @doc """
  Calculates the bitwise XOR of its arguments.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bxor(9, 3)
      10

  """
  @doc guard: true
  @spec bxor(integer, integer) :: integer
  def bxor(left, right) do
    :erlang.bxor(left, right)
  end

  @doc false
  def unquote(:^^^)(left, right) do
    :erlang.bxor(left, right)
  end

  @doc """
  Calculates the result of an arithmetic left bitshift.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bsl(1, 2)
      4

      iex> bsl(1, -2)
      0

      iex> bsl(-1, 2)
      -4

      iex> bsl(-1, -2)
      -1

  """
  @doc guard: true
  @spec bsl(integer, integer) :: integer
  def bsl(left, right) do
    :erlang.bsl(left, right)
  end

  @doc """
  Arithmetic left bitshift operator.

  Calculates the result of an arithmetic left bitshift.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 <<< 2
      4

      iex> 1 <<< -2
      0

      iex> -1 <<< 2
      -4

      iex> -1 <<< -2
      -1

  """
  @doc guard: true
  @spec integer <<< integer :: integer
  def left <<< right do
    :erlang.bsl(left, right)
  end

  @doc """
  Calculates the result of an arithmetic right bitshift.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> bsr(1, 2)
      0

      iex> bsr(1, -2)
      4

      iex> bsr(-1, 2)
      -1

      iex> bsr(-1, -2)
      -4

  """
  @doc guard: true
  @spec bsr(integer, integer) :: integer
  def bsr(left, right) do
    :erlang.bsr(left, right)
  end

  @doc """
  Arithmetic right bitshift operator.

  Calculates the result of an arithmetic right bitshift.

  Allowed in guard tests. Inlined by the compiler.

  ## Examples

      iex> 1 >>> 2
      0

      iex> 1 >>> -2
      4

      iex> -1 >>> 2
      -1

      iex> -1 >>> -2
      -4

  """
  @doc guard: true
  @spec integer >>> integer :: integer
  def left >>> right do
    :erlang.bsr(left, right)
  end
end
