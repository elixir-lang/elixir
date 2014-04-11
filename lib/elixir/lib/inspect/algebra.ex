defmodule Inspect.Algebra do
  @moduledoc ~S"""
  A set of functions for creating and manipulating algebra
  documents, as described in ["Strictly Pretty" (2000) by Christian Lindig][0].

  An algebra document is represented by an `Inspect.Algebra` node
  or a regular string.

      iex> Inspect.Algebra.empty
      :doc_nil

      iex> "foo"
      "foo"

  With the functions in this module, we can concatenate different
  elements together and render them:

      iex> doc = Inspect.Algebra.concat(Inspect.Algebra.empty, "foo")
      iex> Inspect.Algebra.pretty(doc, 80)
      "foo"

  The functions `nest/2`, `space/2` and `line/2` help you put the
  document together into a rigid structure. However, the document
  algebra gets interesting when using functions like `break/2`, which
  converts the given string into a line break depending on how much space
  there is to print. Let's glue two docs together with a break and then
  render it:

      iex> doc = Inspect.Algebra.glue("a", " ", "b")
      iex> Inspect.Algebra.pretty(doc, 80)
      "a b"

  Notice the break was represented as is, because we haven't reached
  a line limit. Once we do, it is replaced by a newline:

      iex> doc = Inspect.Algebra.glue(String.duplicate("a", 20), " ", "b")
      iex> Inspect.Algebra.pretty(doc, 10)
      "aaaaaaaaaaaaaaaaaaaa\nb"

  Finally, this module also contains Elixir related functions, a bit
  tied to Elixir formatting, namely `surround/3` and `surround_many/5`.

  ## Implementation details

  The original Haskell implementation of the algorithm by [Wadler][1]
  relies on lazy evaluation to unfold document groups on two alternatives:
  `:flat` (breaks as spaces) and `:break` (breaks as newlines).
  Implementing the same logic in a strict language such as Elixir leads
  to an exponential growth of possible documents, unless document groups
  are encoded explictly as `:flat` or `:break`. Those groups are then reduced
  to a simple document, where the layout is already decided, per [Lindig][0].

  This implementation slightly changes the semantic of Lindig's algorithm
  to allow elements that belong to the same group to be printed together
  in the same line, even if they do not fit the line fully. This was achieved
  by changing `:break` to mean a possible break and `:flat` to force a flat
  structure. Then deciding if a break works as a newline is just a matter
  of checking if we have enough space until the next break that is not
  inside a group (which is still flat).

  Custom pretty printers can be implemented using the documents returned
  by this module and by providing their own rendering functions.

    [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
    [1]: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

  """

  @surround_separator ","
  @tail_separator " |"
  @newline "\n"
  @nesting 1
  @break " "

  # Functional interface to `doc` records

  @type t :: :doc_nil | :doc_line | doc_cons_t | doc_nest_t | doc_break_t | doc_group_t | binary
  defrecordp :doc_cons, left: :doc_nil :: t, right: :doc_nil :: t
  defrecordp :doc_nest, indent: 1 :: non_neg_integer, doc: :doc_nil :: t
  defrecordp :doc_break, str: " " :: binary
  defrecordp :doc_group, doc: :doc_nil :: t

  defmacrop is_doc(doc) do
    if __CALLER__.in_guard? do
      do_is_doc(doc)
    else
      var = quote do: doc
      quote do
        unquote(var) = unquote(doc)
        unquote(do_is_doc(var))
      end
    end
  end

  defp do_is_doc(doc) do
    quote do
      is_binary(unquote(doc)) or
      unquote(doc) in [:doc_nil, :doc_line] or
      (is_tuple(unquote(doc)) and
       elem(unquote(doc), 0) in [:doc_cons, :doc_nest, :doc_break, :doc_group])
    end
  end

  @doc """
  Converts an Elixir structure to an algebra document
  according to the inspect protocol.
  """
  @spec to_doc(any, Inspect.Opts.t) :: t
  def to_doc(arg, opts) when is_record(arg) and is_record(opts, Inspect.Opts) do
    if opts.records do
      try do
        Inspect.inspect(arg, opts)
      rescue
        e ->
          res = Inspect.Tuple.inspect(arg, opts)
          IO.puts :stderr, "** (Inspect.Error) Got #{inspect e.__record__(:name)} with message " <>
                           "#{e.message} while inspecting #{pretty(res, opts.width)}"
          res
      end
    else
      Inspect.Tuple.inspect(arg, opts)
    end
  end

  def to_doc(%{ __struct__: struct } = map, opts) when is_atom(struct) and is_record(opts, Inspect.Opts) do
    if opts.structs do
      try do
        Inspect.inspect(map, opts)
      rescue
        e ->
          res = Inspect.Map.inspect(map, opts)
          IO.puts :stderr, "** (Inspect.Error) Got #{inspect e.__record__(:name)} with message " <>
                           "#{e.message} while inspecting #{pretty(res, opts.width)}"
          res
      end
    else
      Inspect.Map.inspect(map, opts)
    end
  end

  def to_doc(arg, opts) when is_record(opts, Inspect.Opts) do
    Inspect.inspect(arg, opts)
  end

  @doc """
  Returns `:doc_nil` which is a document entity used to represent
  nothingness. Takes no arguments.

  ## Examples

      iex> Inspect.Algebra.empty
      :doc_nil

  """
  @spec empty() :: :doc_nil
  def empty, do: :doc_nil

  @doc """
  Concatenates two document entities. Takes two arguments:
  left doc and right doc. Returns a DocCons doc

  ## Examples

      iex> doc = Inspect.Algebra.concat "Tasteless", "Artosis"
      iex> Inspect.Algebra.pretty(doc, 80)
      "TastelessArtosis"

  """
  @spec concat(t, t) :: doc_cons_t
  def concat(x, y) when is_doc(x) and is_doc(y) do
    doc_cons(left: x, right: y)
  end

  @doc """
  Concatenates a list of documents.
  """
  @spec concat([t]) :: doc_cons_t
  def concat(docs) do
    folddoc(docs, &concat(&1, &2))
  end

  @doc """
  Nests document entity `x` positions deep. Nesting will be
  appended to the line breaks.

  ## Examples

      iex> doc = Inspect.Algebra.nest(Inspect.Algebra.concat(Inspect.Algebra.break, "6"), 5)
      iex> Inspect.Algebra.pretty(doc, 80)
      " 6"

  """
  @spec nest(t, non_neg_integer) :: doc_nest_t
  def nest(x, 0) when is_doc(x) do
    x
  end

  def nest(x, i) when is_doc(x) and is_integer(i) do
    doc_nest(indent: i, doc: x)
  end

  @doc ~S"""
  Document entity representing a break. This break can
  be rendered as a linebreak or as spaces, depending on the
  `mode` of the chosen layout or the provided separator.

  ## Examples

  Let's glue two docs together with a break and then render it:

      iex> doc = Inspect.Algebra.glue("a", " ", "b")
      iex> Inspect.Algebra.pretty(doc, 80)
      "a b"

  Notice the break was represented as is, because we haven't reached
  a line limit. Once we do, it is replaced by a newline:

      iex> doc = Inspect.Algebra.glue(String.duplicate("a", 20), " ", "b")
      iex> Inspect.Algebra.pretty(doc, 10)
      "aaaaaaaaaaaaaaaaaaaa\nb"

  """
  @spec break(binary) :: doc_break_t
  def break(s) when is_binary(s), do: doc_break(str: s)

  @spec break() :: doc_break_t
  def break(), do: doc_break(str: @break)

  @doc """
  Inserts a break between two docs. See `break/1` for more info.
  """
  @spec glue(t, t) :: doc_cons_t
  def glue(x, y), do: concat(x, concat(break, y))

  @doc """
  Inserts a break, passed as the second argument, between two docs,
  the first and the third arguments.
  """
  @spec glue(t, binary, t) :: doc_cons_t
  def glue(x, g, y) when is_binary(g), do: concat(x, concat(break(g), y))

  @doc ~S"""
  Returns a group containing the specified document.

  ## Examples

      iex> doc = Inspect.Algebra.group(
      ...>   Inspect.Algebra.concat(
      ...>     Inspect.Algebra.group(
      ...>       Inspect.Algebra.concat(
      ...>         "Hello,",
      ...>         Inspect.Algebra.concat(
      ...>           Inspect.Algebra.break,
      ...>           "A"
      ...>         )
      ...>       )
      ...>     ),
      ...>     Inspect.Algebra.concat(
      ...>       Inspect.Algebra.break,
      ...>       "B"
      ...>     )
      ...> ))
      iex> Inspect.Algebra.pretty(doc, 80)
      "Hello, A B"
      iex> Inspect.Algebra.pretty(doc, 6)
      "Hello,\nA B"

  """
  @spec group(t) :: doc_group_t
  def group(d) when is_doc(d) do
    doc_group(doc: d)
  end

  @doc """
  Inserts a mandatory single space between two document entities.

  ## Examples

      iex> doc = Inspect.Algebra.space "Hughes", "Wadler"
      iex> Inspect.Algebra.pretty(doc, 80)
      "Hughes Wadler"

  """
  @spec space(t, t) :: doc_cons_t
  def space(x, y), do: concat(x, concat(" ", y))

  @doc ~S"""
  Inserts a mandatory linebreak between two document entities.

  ## Examples

      iex> doc = Inspect.Algebra.line "Hughes", "Wadler"
      iex> Inspect.Algebra.pretty(doc, 80)
      "Hughes\nWadler"

  """
  @spec line(t, t) :: doc_cons_t
  def line(x, y), do: concat(x, concat(:doc_line, y))

  @doc """
  Folds a list of document entities into a document entity
  using a function that is passed as the first argument.

  ## Examples

      iex> doc = ["A", "B"]
      iex> doc = Inspect.Algebra.folddoc(doc, fn(x,y) ->
      ...>   Inspect.Algebra.concat [x, "!", y]
      ...> end)
      iex> Inspect.Algebra.pretty(doc, 80)
      "A!B"

  """
  @spec folddoc([t], ((t, t) -> t)) :: t
  def folddoc([], _), do: empty
  def folddoc([doc], _), do: doc
  def folddoc([d|ds], f), do: f.(d, folddoc(ds, f))

  # Elixir conveniences

  @doc ~S"""
  Surrounds a document with characters.

  Puts the document between left and right enclosing and nesting it.
  The document is marked as a group, to show the maximum as possible
  concisely together.

  ## Examples

      iex> doc = Inspect.Algebra.surround "[", Inspect.Algebra.glue("a", "b"), "]"
      iex> Inspect.Algebra.pretty(doc, 3)
      "[a\n b]"

  """
  @spec surround(binary, t, binary) :: t
  def surround(left, doc, right) do
    group concat left, concat(nest(doc, @nesting), right)
  end

  @doc ~S"""
  Maps and glues a collection of items together using the given separator
  and surrounds them. A limit can be passed which, once reached, stops
  gluing and outputs "..." instead.

  ## Examples

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]", :infinity, &integer_to_binary(&1))
      iex> Inspect.Algebra.pretty(doc, 5)
      "[1,\n 2,\n 3,\n 4,\n 5]"

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]", 3, &integer_to_binary(&1))
      iex> Inspect.Algebra.pretty(doc, 20)
      "[1, 2, 3, ...]"

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]", 3, &integer_to_binary(&1), "!")
      iex> Inspect.Algebra.pretty(doc, 20)
      "[1! 2! 3! ...]"

  """
  @spec surround_many(binary, [any], binary, integer | :infinity, (term -> t), binary) :: t
  def surround_many(left, docs, right, limit, fun, separator \\ @surround_separator)

  def surround_many(left, [], right, _, _fun, _) do
    concat(left, right)
  end

  def surround_many(left, docs, right, limit, fun, sep) do
    surround(left, surround_many(docs, limit, fun, sep), right)
  end

  defp surround_many(_, 0, _fun, _sep) do
    "..."
  end

  defp surround_many([h], _limit, fun, _sep) do
    fun.(h)
  end

  defp surround_many([h|t], limit, fun, sep) when is_list(t) do
    glue(
      concat(fun.(h), sep),
      surround_many(t, decrement(limit), fun, sep)
    )
  end

  defp surround_many([h|t], _limit, fun, _sep) do
    glue(
      concat(fun.(h), @tail_separator),
      fun.(t)
    )
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1

  @doc """
  The pretty printing function.

  Takes the maximum width and a document to print as its arguments
  and returns the string representation of the best layout for the
  document to fit in the given width.
  """
  @spec pretty(t, non_neg_integer | :infinity) :: binary
  def pretty(d, w) do
    sdoc = format w, 0, [{0, default_mode(w), doc_group(doc: d)}]
    render(sdoc)
  end

  defp default_mode(:infinity), do: :flat
  defp default_mode(_),         do: :break

  # Rendering and internal helpers

  # Record representing the document mode to be rendered: flat or broken
  @typep mode :: :flat | :break

  @doc false
  @spec fits?(integer, [{ integer, mode, t }]) :: boolean
  def fits?(w, _) when w < 0,                               do: false
  def fits?(_, []),                                         do: true
  def fits?(_, [{_, _, :doc_line} | _]),                    do: true
  def fits?(w, [{_, _, :doc_nil} | t]),                     do: fits?(w, t)
  def fits?(w, [{i, m, doc_cons(left: x, right: y)} | t]),  do: fits?(w, [{i, m, x} | [{i, m, y} | t]])
  def fits?(w, [{i, m, doc_nest(indent: j, doc: x)} | t]),  do: fits?(w, [{i + j, m, x} | t])
  def fits?(w, [{i, _, doc_group(doc: x)} | t]),            do: fits?(w, [{i, :flat, x} | t])
  def fits?(w, [{_, _, s} | t]) when is_binary(s),          do: fits?((w - byte_size s), t)
  def fits?(w, [{_, :flat, doc_break(str: s)} | t]),        do: fits?((w - byte_size s), t)
  def fits?(_, [{_, :break, doc_break(str: _)} | _]),       do: true

  @doc false
  @spec format(integer | :infinity, integer, [{ integer, mode, t }]) :: [binary]
  def format(_, _, []),                                        do: []
  def format(w, _, [{i, _, :doc_line} | t]),                   do: [indent(i) | format(w, i, t)]
  def format(w, k, [{_, _, :doc_nil} | t]),                    do: format(w, k, t)
  def format(w, k, [{i, m, doc_cons(left: x, right: y)} | t]), do: format(w, k, [{i, m, x} | [{i, m, y} | t]])
  def format(w, k, [{i, m, doc_nest(indent: j, doc: x)} | t]), do: format(w, k, [{i + j, m, x} | t])
  def format(w, k, [{i, m, doc_group(doc: x)} | t]),           do: format(w, k, [{i, m, x} | t])
  def format(w, k, [{_, _, s} | t]) when is_binary(s),         do: [s | format(w, (k + byte_size s), t)]
  def format(w, k, [{_, :flat, doc_break(str: s)} | t]),       do: [s | format(w, (k + byte_size s), t)]
  def format(w, k, [{i, :break, doc_break(str: s)} | t]) do
    k = k + byte_size(s)

    if w == :infinity or fits?(w - k, t) do
      [s | format(w, k, t)]
    else
      [indent(i) | format(w, i, t)]
    end
  end

  defp indent(0), do: @newline
  defp indent(i), do: @newline <> :binary.copy(" ", i)

  @doc false
  @spec render([binary]) :: binary
  def render(sdoc) do
    iolist_to_binary sdoc
  end
end
