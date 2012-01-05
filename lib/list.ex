defmodule List do
  def append(list) do
    Erlang.lists.append(list)
  end

  def append([h], right) do
    [h|right]
  end

  def append(left, right) do
    left ++ right
  end

  def foldl([h|t], acc, f) do
    foldl(t, f.(h, acc), f)
  end

  def foldl([], acc, f) when is_function(f, 2) do
    acc
  end

  def each(list, f) do
    _each(list, f)
    list
  end

  # Join the given `list` according to `joiner`.
  # Joiner can be either a binary or a list and the
  # result will be of the same type of joiner.
  #
  # == Examples
  #
  #     List.join([1,2,3], " = ") #=> "1 = 2 = 3"
  #     List.join([1,2,3], ' = ') #=> '1 = 2 = 3'
  #
  def join(list, joiner) when is_list(joiner) do
    binary_to_list join(list, list_to_binary(joiner))
  end

  def join(list, joiner) do
    _join(list, joiner, "")
  end

  def map([h|t], f) do
    [f.(h)|map(t,f)]
  end

  def map([], f) when is_function(f, 1) do
    []
  end

  def mapfoldl([h|t], acc, f) do
    { result, acc } = f.(h, acc)
    { rest, acc }   = mapfoldl(t, acc, f)
    { [result|rest], acc }
  end

  def mapfoldl([], acc, f) when is_function(f, 2) do
    { [], acc }
  end

  def reverse(list) do
    Erlang.lists.reverse(list)
  end

  def uniq(list) do
    _uniq(list, [])
  end

  def wrap(list) when is_list(list) do
    list
  end

  def wrap(nil),   do: []
  def wrap(other), do: [other]

  ## Private

  defp _each([h|t], f) do
    f.(h)
    _each(t, f)
  end

  defp _each([], f) when is_function(f, 1) do
    []
  end

  def _join([h], _joiner, acc) do
    << acc | :binary, stringify(h) | :binary >>
  end

  def _join([h|t], joiner, acc) do
    acc = << acc | :binary, stringify(h) | :binary, joiner | :binary >>
    _join(t, joiner, acc)
  end

  def _join([], _joiner, _acc) do
    ""
  end

  defp _uniq([h|t], acc) do
    case Erlang.lists.member(h, acc) do
    match: true
      _uniq(t, acc)
    match: false
      _uniq(t, [h|acc])
    end
  end

  defp _uniq([], acc) do
    Erlang.lists.reverse(acc)
  end
end