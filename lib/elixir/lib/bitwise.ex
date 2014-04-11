defmodule Bitwise do
  @moduledoc """
  This module provide macros and operators for bitwise operators.
  These macros can be used in guards.

  The easiest way to use is to simply import them into
  your module:

      iex> use Bitwise
      iex> bnot 1
      -2
      iex> 1 &&& 1
      1

  You can select to include only or skip operators by passing options:

      iex> use Bitwise, only_operators: true
      iex> 1 &&& 1
      1

  """

  @doc """
  Allow a developer to use this module in their programs with
  the following options:

  * `:only_operators` - Include only operators;
  * `:skip_operators` - Skip operators;

  """
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
  Bitwise not.
  """
  defmacro bnot(expr) do
    quote do: :erlang.bnot(unquote(expr))
  end

  @doc """
  Bitwise not as operator.
  """
  defmacro ~~~expr do
    quote do: :erlang.bnot(unquote(expr))
  end

  @doc """
  Bitwise and.
  """
  defmacro band(left, right) do
    quote do: :erlang.band(unquote(left), unquote(right))
  end

  @doc """
  Bitwise and as operator.
  """
  defmacro left &&& right do
    quote do: :erlang.band(unquote(left), unquote(right))
  end

  @doc """
  Bitwise or.
  """
  defmacro bor(left, right) do
    quote do: :erlang.bor(unquote(left), unquote(right))
  end

  @doc """
  Bitwise or as operator.
  """
  defmacro left ||| right do
    quote do: :erlang.bor(unquote(left), unquote(right))
  end

  @doc """
  Bitwise xor.
  """
  defmacro bxor(left, right) do
    quote do: :erlang.bxor(unquote(left), unquote(right))
  end

  @doc """
  Bitwise xor as operator.
  """
  defmacro left ^^^ right do
    quote do: :erlang.bxor(unquote(left), unquote(right))
  end

  @doc """
  Arithmetic bitshift left.
  """
  defmacro bsl(left, right) do
    quote do: :erlang.bsl(unquote(left), unquote(right))
  end

  @doc """
  Arithmetic bitshift left as operator.
  """
  defmacro left <<< right do
    quote do: :erlang.bsl(unquote(left), unquote(right))
  end

  @doc """
  Arithmetic bitshift right.
  """
  defmacro bsr(left, right) do
    quote do: :erlang.bsr(unquote(left), unquote(right))
  end

  @doc """
  Arithmetic bitshift right as operator.
  """
  defmacro left >>> right do
    quote do: :erlang.bsr(unquote(left), unquote(right))
  end
end
