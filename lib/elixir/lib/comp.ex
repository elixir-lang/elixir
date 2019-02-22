defprotocol Comparable do
  @moduledoc """
  Protocol that describes comparison of two terms
  """

  @fallback_to_any true
  @type t :: Comparable.t()
  @type ord :: :gt | :lt | :eq

  @doc """
  Function to compare term or given type with any other term

  ## Examples

  ```
  iex> 2 > 1
  true
  iex> Comparable.compare(2, 1)
  :gt
  iex> 1 < 2
  true
  iex> Comparable.compare(1, 2)
  :lt
  iex> 1 == 1
  true
  iex> Comparable.compare(1, 1)
  :eq

  iex> {1, 2, 3} > {1, 2}
  true
  iex> Comparable.compare({1, 2, 3}, {1, 2})
  :gt
  iex> {1, 2} < {1, 2, 3}
  true
  iex> Comparable.compare({1, 2}, {1, 2, 3})
  :lt
  iex> {1, 2} == {1, 2}
  true
  iex> Comparable.compare({1, 2}, {1, 2})
  :eq

  iex> {1, 2, 3} > {1, 1000}
  true
  iex> Comparable.compare({1, 2, 3}, {1, 1000})
  :gt
  iex> {1, 1000} < {1, 2, 3}
  true
  iex> Comparable.compare({1, 1000}, {1, 2, 3})
  :lt
  iex> {1, 2} < {1, 1000}
  true
  iex> Comparable.compare({1, 2}, {1, 1000})
  :lt

  iex> [1, 2, 3] > [1, 2]
  true
  iex> Comparable.compare([1, 2, 3], [1, 2])
  :gt
  iex> [1, 2] < [1, 2, 3]
  true
  iex> Comparable.compare([1, 2], [1, 2, 3])
  :lt
  iex> [1, 2] == [1, 2]
  true
  iex> Comparable.compare([1, 2], [1, 2])
  :eq

  iex> [1, 2, 3] < [1, 1000]
  true
  iex> Comparable.compare([1, 2, 3], [1, 1000])
  :lt
  iex> [1, 1000] > [1, 2, 3]
  true
  iex> Comparable.compare([1, 1000], [1, 2, 3])
  :gt
  iex> [1, 2] < [1, 1000]
  true
  iex> Comparable.compare([1, 2], [1, 1000])
  :lt

  iex> %{z: 1000} < %{a: 1, b: 1}
  true
  iex> Comparable.compare(%{z: 1000}, %{a: 1, b: 1})
  :lt
  iex> %{a: 1, z: 1} > %{a: 1000, b: 1000}
  true
  iex> Comparable.compare(%{a: 1, z: 1}, %{a: 1000, b: 1000})
  :gt
  iex> %{a: 1, b: 1} == %{a: 1, b: 1}
  true
  iex> Comparable.compare(%{a: 1, b: 1}, %{a: 1, b: 1})
  :eq

  iex> %{a: 1, b: 1, c: 1000} < %{a: 1, b: 1000, c: 1}
  true
  iex> Comparable.compare(%{a: 1, b: 1, c: 1000}, %{a: 1, b: 1000, c: 1})
  :lt

  iex> {:ok, dt} = NaiveDateTime.new(2000, 1, 1, 0, 0, 0)
  {:ok, ~N[2000-01-01 00:00:00]}
  iex> NaiveDateTime.compare(NaiveDateTime.add(dt, 1, :microsecond), NaiveDateTime.add(dt, 1, :second))
  :lt
  iex> Comparable.compare(NaiveDateTime.add(dt, 1, :microsecond), NaiveDateTime.add(dt, 1, :second))
  :lt

  iex> Comparable.compare(%URI{host: "1"}, %URI{host: "2"})
  :lt
  iex> {:ok, dt} = NaiveDateTime.new(2000, 1, 1, 0, 0, 0)
  {:ok, ~N[2000-01-01 00:00:00]}
  iex> Comparable.compare(%URI{host: "1"}, dt)
  :lt
  iex> Comparable.compare(%URI{host: "1"}, self())
  :gt
  iex> Comparable.compare(self(), %URI{host: "1"})
  :lt
  ```
  """
  @spec compare(t, term) :: ord
  def compare(t, term)
end

defmodule Comp do
  @moduledoc """
  Provides utilities to implement `Comparable` protocol and work with `Comparable` types
  """

  @type left :: Comparable.t()
  @type right :: term

  defmacro gt, do: :gt
  defmacro lt, do: :lt
  defmacro eq, do: :eq

  @doc """
  Helper to auto-generate clauses where right term may have a type other than left term
  """
  defmacro deffallback do
    quote do
      def compare(left, right) when left > right, do: unquote(__MODULE__).gt()
      def compare(left, right) when left < right, do: unquote(__MODULE__).lt()
    end
  end

  @doc """
  Enables <~>, <|>, >>>, <<<, ~>>, <<~ infix shortcuts and
  Comp.gt, Comp.lt, Comp.eq macro
  """
  defmacro __using__(_) do
    quote do
      require Comp
      import Comp, only: [<~>: 2, <|>: 2, >>>: 2, <<<: 2, ~>>: 2, <<~: 2]
    end
  end

  @doc """
  Infix shortcut for `Comp.equal?/2`

  ## Examples

  ```
  iex> use Comp
  Comp
  iex> 1 <~> 1
  true
  iex> 1 <~> :hello
  false
  ```
  """
  defmacro left <~> right do
    quote do
      unquote(left)
      |> Comp.equal?(unquote(right))
    end
  end

  @doc """
  Infix shortcut for `Comp.not_equal?/2`

  ## Examples

  ```
  iex> use Comp
  Comp
  iex> 1 <|> 1
  false
  iex> 1 <|> :hello
  true
  ```
  """
  defmacro left <|> right do
    quote do
      unquote(left)
      |> Comp.not_equal?(unquote(right))
    end
  end

  @doc """
  Infix shortcut for `Comp.greater_than?/2`

  ## Examples

  ```
  iex> use Comp
  Comp
  iex> 1 >>> 1
  false
  iex> 1 >>> 2
  false
  iex> 2 >>> 1
  true
  ```
  """
  defmacro left >>> right do
    quote do
      unquote(left)
      |> Comp.greater_than?(unquote(right))
    end
  end

  @doc """
  Infix shortcut for `Comp.less_than?/2`

  ## Examples

  ```
  iex> use Comp
  Comp
  iex> 1 <<< 1
  false
  iex> 1 <<< 2
  true
  iex> 2 <<< 1
  false
  ```
  """
  defmacro left <<< right do
    quote do
      unquote(left)
      |> Comp.less_than?(unquote(right))
    end
  end

  @doc """
  Infix shortcut for `Comp.greater_or_equal?/2`

  ## Examples

  ```
  iex> use Comp
  Comp
  iex> 1 ~>> 1
  true
  iex> 1 ~>> 2
  false
  iex> 2 ~>> 1
  true
  ```
  """
  defmacro left ~>> right do
    quote do
      unquote(left)
      |> Comp.greater_or_equal?(unquote(right))
    end
  end

  @doc """
  Infix shortcut for `Comp.less_or_equal?/2`

  ## Examples

  ```
  iex> use Comp
  Comp
  iex> 1 <<~ 1
  true
  iex> 1 <<~ 2
  true
  iex> 2 <<~ 1
  false
  ```
  """
  defmacro left <<~ right do
    quote do
      unquote(left)
      |> Comp.less_or_equal?(unquote(right))
    end
  end

  @doc """
  Is left term equal to right term?

  ## Examples

  ```
  iex> Comp.equal?(1, 1)
  true
  iex> Comp.equal?(1, :hello)
  false
  ```
  """
  @spec equal?(left, right) :: boolean
  def equal?(left, right) do
    Comparable.compare(left, right) == eq()
  end

  @doc """
  Is left term not equal to right term?

  ## Examples

  ```
  iex> Comp.not_equal?(1, 1)
  false
  iex> Comp.not_equal?(1, :hello)
  true
  ```
  """
  @spec not_equal?(left, right) :: boolean
  def not_equal?(left, right) do
    Comparable.compare(left, right) != eq()
  end

  @doc """
  Is left term greater than right term?

  ## Examples

  ```
  iex> Comp.greater_than?(1, 1)
  false
  iex> Comp.greater_than?(1, 2)
  false
  iex> Comp.greater_than?(2, 1)
  true
  """
  @spec greater_than?(left, right) :: boolean
  def greater_than?(left, right) do
    Comparable.compare(left, right) == gt()
  end

  @doc """
  Is left term less than right term?

  ## Examples

  ```
  iex> Comp.less_than?(1, 1)
  false
  iex> Comp.less_than?(1, 2)
  true
  iex> Comp.less_than?(2, 1)
  false
  """
  @spec less_than?(left, right) :: boolean
  def less_than?(left, right) do
    Comparable.compare(left, right) == lt()
  end

  @doc """
  Is left term greater or equal to right term?

  ## Examples

  ```
  iex> Comp.greater_or_equal?(1, 1)
  true
  iex> Comp.greater_or_equal?(1, 2)
  false
  iex> Comp.greater_or_equal?(2, 1)
  true
  """
  @spec greater_or_equal?(left, right) :: boolean
  def greater_or_equal?(left, right) do
    Comparable.compare(left, right) != lt()
  end

  @doc """
  Is left term less or equal to right term?

  ## Examples

  ```
  iex> Comp.less_or_equal?(1, 1)
  true
  iex> Comp.less_or_equal?(1, 2)
  true
  iex> Comp.less_or_equal?(2, 1)
  false
  """
  @spec less_or_equal?(left, right) :: boolean
  def less_or_equal?(left, right) do
    Comparable.compare(left, right) != gt()
  end

  @doc """
  Returns the biggest of the two given terms, if terms are equal - then the first one is returned

  ## Examples

  ```
  iex> Comp.max(1, 1)
  1
  iex> Comp.max(1, 2)
  2
  iex> Comp.max(2, 1)
  2
  ```
  """
  @spec max(left, right) :: left | right
  def max(left, right) do
    left
    |> Comparable.compare(right)
    |> case do
      gt() -> left
      lt() -> right
      eq() -> left
    end
  end

  @doc """
  Returns the smallest of the two given terms, if terms are equal - then the first one is returned

  ## Examples

  ```
  iex> Comp.min(1, 1)
  1
  iex> Comp.min(1, 2)
  1
  iex> Comp.min(2, 1)
  1
  ```
  """
  @spec min(left, right) :: left | right
  def min(left, right) do
    left
    |> Comparable.compare(right)
    |> case do
      gt() -> right
      lt() -> left
      eq() -> left
    end
  end

  @doc """
  Compare left and right term

  ## Examples

  ```
  iex> Comp.compare(1, 2)
  :lt
  iex> Comp.compare(2, 1)
  :gt
  iex> Comp.compare(1, 1)
  :eq
  ```
  """
  @spec compare(left, right) :: Comparable.ord()
  def compare(left, right) do
    Comparable.compare(left, right)
  end
end

[
  Atom,
  BitString,
  Float,
  Function,
  Integer,
  PID,
  Port,
  Reference
]
|> Enum.each(fn t ->
  defimpl Comparable, for: t do
    require Comp

    def compare(left, right) do
      cond do
        left > right -> Comp.gt()
        left < right -> Comp.lt()
        true -> Comp.eq()
      end
    end
  end
end)

defimpl Comparable, for: Tuple do
  require Comp

  def compare(left, right) when is_tuple(right) do
    left_length = tuple_size(left)
    right_length = tuple_size(right)

    cond do
      left_length > right_length ->
        Comp.gt()

      left_length < right_length ->
        Comp.lt()

      true ->
        left
        |> Tuple.to_list()
        |> Comparable.compare(right |> Tuple.to_list())
    end
  end

  Comp.deffallback()
end

defimpl Comparable, for: Map do
  require Comp

  def compare(left, %{} = right) do
    left_length = map_size(left)
    right_length = map_size(right)

    cond do
      left_length > right_length ->
        Comp.gt()

      left_length < right_length ->
        Comp.lt()

      true ->
        left
        |> Map.keys()
        |> Comparable.compare(right |> Map.keys())
        |> case do
          res when res in [Comp.gt(), Comp.lt()] ->
            res

          Comp.eq() ->
            left
            |> Map.values()
            |> Comparable.compare(right |> Map.values())
        end
    end
  end

  Comp.deffallback()
end

defimpl Comparable, for: List do
  require Comp

  def compare(left, right) when is_list(right) do
    left
    |> Stream.zip(right)
    |> Enum.reduce_while(Comp.eq(), fn {lx, rx}, Comp.eq() ->
      lx
      |> Comparable.compare(rx)
      |> case do
        res when res in [Comp.gt(), Comp.lt()] -> {:halt, res}
        Comp.eq() = res -> {:cont, res}
      end
    end)
    |> case do
      res when res in [Comp.gt(), Comp.lt()] ->
        res

      Comp.eq() ->
        left_length = length(left)
        right_length = length(right)

        cond do
          left_length > right_length -> Comp.gt()
          left_length < right_length -> Comp.lt()
          true -> Comp.eq()
        end
    end
  end

  Comp.deffallback()
end

defimpl Comparable, for: Any do
  require Comp

  def compare(%struct{} = left, %struct{} = right) do
    left
    |> Map.from_struct()
    |> Comparable.compare(right |> Map.from_struct())
  end

  Comp.deffallback()
end

defimpl Comparable, for: NaiveDateTime do
  require Comp

  def compare(left, %NaiveDateTime{} = right) do
    NaiveDateTime.compare(left, right)
  end

  Comp.deffallback()
end
