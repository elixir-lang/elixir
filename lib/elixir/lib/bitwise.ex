defmodule Bitwise do
  @moduledoc """
  This module provides macro-based operators that perform calculations
  on (sets of) bits.

  In general, you should `use` the Bitwise module as a whole:

      iex> use Bitwise
      iex> bnot 1
      -2
      iex> 1 &&& 1
      1

  When used, it accepts the following options:

    * `:only_operators` - include only operators
    * `:skip_operators` - skip operators


      iex> use Bitwise, only_operators: true
      iex> 1 &&& 1
      1


  These macros can be used in guards:

      iex> use Bitwise
      iex> odd? = fn(int) when band(int, 1) == 1 -> true; (_) -> false end
      iex> odd?.(1)
      true

  """

  @doc false
  defmacro __using__(options) do
    except = cond do
      Keyword.get(options, :only_operators) ->
        [bnot: 1, band: 2, bor: 2, bxor: 2, bsl: 2, bsr: 2]
      Keyword.get(options, :skip_operators) ->
        [~~~: 1, &&&: 2, |||: 2, ^^^: 2, <<<: 2, >>>: 2]
      true -> []
    end

    quote do
      import Bitwise, except: unquote(except)
    end
  end

  @doc """
  Calculates the bitwise NOT of its argument.

      iex> bnot(2)
      -3
      iex> bnot(2) &&& 3
      1

  """
  defmacro bnot(expr) do
    quote do: :erlang.bnot(unquote(expr))
  end

  @doc """
  Prefix (unary) operator; calculates the bitwise NOT of its argument.

      iex> ~~~2
      -3
      iex> ~~~2 &&& 3
      1

  """
  defmacro ~~~expr do
    quote do: :erlang.bnot(unquote(expr))
  end

  @doc """
  Calculates the bitwise AND of its arguments.

      iex> band(9, 3)
      1

  """
  defmacro band(left, right) do
    quote do: :erlang.band(unquote(left), unquote(right))
  end

  @doc """
  Infix operator; calculates the bitwise AND of its arguments.

      iex> 9 &&& 3
      1

  """
  defmacro left &&& right do
    quote do: :erlang.band(unquote(left), unquote(right))
  end

  @doc """
  Calculates the bitwise OR of its arguments.

      iex> bor(9, 3)
      11

  """
  defmacro bor(left, right) do
    quote do: :erlang.bor(unquote(left), unquote(right))
  end

  @doc """
  Infix operator; calculates the bitwise OR of its arguments.

      iex> 9 ||| 3
      11

  """
  defmacro left ||| right do
    quote do: :erlang.bor(unquote(left), unquote(right))
  end

  @doc """
  Calculates the bitwise XOR of its arguments.

      iex> bxor(9, 3)
      10

  """
  defmacro bxor(left, right) do
    quote do: :erlang.bxor(unquote(left), unquote(right))
  end

  @doc """
  Infix operator; calculates the bitwise XOR of its arguments.

      iex> 9 ^^^ 3
      10

  """
  defmacro left ^^^ right do
    quote do: :erlang.bxor(unquote(left), unquote(right))
  end

  @doc """
  Calculates the result of an arithmetic left bitshift.

      iex> bsl(1, 2)
      4
      iex> bsl(1, -2)
      0
      iex> bsl(-1, 2)
      -4
      iex> bsl(-1, -2)
      -1

  """
  defmacro bsl(left, right) do
    quote do: :erlang.bsl(unquote(left), unquote(right))
  end

  @doc """
  Infix operator; calculates the result of an arithmetic left bitshift.

      iex> 1 <<< 2
      4
      iex> 1 <<< -2
      0
      iex> -1 <<< 2
      -4
      iex> -1 <<< -2
      -1

  """
  defmacro left <<< right do
    quote do: :erlang.bsl(unquote(left), unquote(right))
  end

  @doc """
  Calculates the result of an arithmetic right bitshift.

      iex> bsr(1, 2)
      0
      iex> bsr(1, -2)
      4
      iex> bsr(-1, 2)
      -1
      iex> bsr(-1, -2)
      -4

  """
  defmacro bsr(left, right) do
    quote do: :erlang.bsr(unquote(left), unquote(right))
  end

  @doc """
  Infix operator; calculates the result of an arithmetic right bitshift.

      iex> 1 >>> 2
      0
      iex> 1 >>> -2
      4
      iex> -1 >>> 2
      -1
      iex> -1 >>> -2
      -4

  """
  defmacro left >>> right do
    quote do: :erlang.bsr(unquote(left), unquote(right))
  end
end
