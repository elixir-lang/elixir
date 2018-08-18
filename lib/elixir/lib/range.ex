defmodule Range do
  @moduledoc """
  Defines a range.

  A range represents a discrete number of values where
  the first and last values are integers.

  Ranges can be either increasing (`first <= last`) or
  decreasing (`first > last`). Ranges are also always
  inclusive.

  A range is represented internally as a struct. However,
  the most common form of creating and matching on ranges
  is via the `../2` macro, auto-imported from `Kernel`:

      iex> range = 1..3
      1..3
      iex> first..last = range
      iex> first
      1
      iex> last
      3

  A range implements the `Enumerable` protocol, which means
  functions in the `Enum` module can be used to work with
  ranges:

      iex> range = 1..10
      1..10
      iex> Enum.reduce(range, 0, fn i, acc -> i * i + acc end)
      385
      iex> Enum.count(range)
      10
      iex> Enum.member?(range, 11)
      false
      iex> Enum.member?(range, 8)
      true

  """

  defstruct first: nil, last: nil

  @type t :: %__MODULE__{first: integer, last: integer}
  @type t(first, last) :: %__MODULE__{first: first, last: last}

  @doc """
  Creates a new range.
  """
  @spec new(integer, integer) :: t
  def new(first, last) when is_integer(first) and is_integer(last) do
    %Range{first: first, last: last}
  end

  def new(first, last) do
    raise ArgumentError,
          "ranges (first..last) expect both sides to be integers, " <>
            "got: #{inspect(first)}..#{inspect(last)}"
  end

  # TODO: Remove by 2.0
  @doc false
  @deprecated "Pattern match on first..last instead"
  def range?(term)
  def range?(first..last) when is_integer(first) and is_integer(last), do: true
  def range?(_), do: false
end

defimpl Enumerable, for: Range do
  def reduce(first..last, acc, fun) do
    reduce(first, last, acc, fun, _up? = last >= first)
  end

  defp reduce(_first, _last, {:halt, acc}, _fun, _up?) do
    {:halted, acc}
  end

  defp reduce(first, last, {:suspend, acc}, fun, up?) do
    {:suspended, acc, &reduce(first, last, &1, fun, up?)}
  end

  defp reduce(first, last, {:cont, acc}, fun, _up? = true) when first <= last do
    reduce(first + 1, last, fun.(first, acc), fun, _up? = true)
  end

  defp reduce(first, last, {:cont, acc}, fun, _up? = false) when first >= last do
    reduce(first - 1, last, fun.(first, acc), fun, _up? = false)
  end

  defp reduce(_, _, {:cont, acc}, _fun, _up) do
    {:done, acc}
  end

  def member?(first..last, value) when is_integer(value) do
    if first <= last do
      {:ok, first <= value and value <= last}
    else
      {:ok, last <= value and value <= first}
    end
  end

  def member?(_.._, _value) do
    {:ok, false}
  end

  def count(first..last) do
    if first <= last do
      {:ok, last - first + 1}
    else
      {:ok, first - last + 1}
    end
  end

  def slice(first..last) do
    if first <= last do
      {:ok, last - first + 1, &slice_asc(first + &1, &2)}
    else
      {:ok, first - last + 1, &slice_desc(first - &1, &2)}
    end
  end

  defp slice_asc(current, 1), do: [current]
  defp slice_asc(current, remaining), do: [current | slice_asc(current + 1, remaining - 1)]

  defp slice_desc(current, 1), do: [current]
  defp slice_desc(current, remaining), do: [current | slice_desc(current - 1, remaining - 1)]
end

defimpl Inspect, for: Range do
  import Inspect.Algebra

  def inspect(first..last, opts) do
    concat([to_doc(first, opts), "..", to_doc(last, opts)])
  end
end
