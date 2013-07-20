defmodule Inspect.Algebra do
  @moduledoc %B"""
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

  defp repeat(_, 0), do: ""
  defp repeat(s, i), do: :lists.duplicate(i, s)

  # Functional interface to `doc` records

  @type t :: :doc_nil | doc_cons_t | doc_nest_t | doc_break_t | doc_group_t | binary
  defrecordp :doc_cons, left: :doc_nil :: t, right: :doc_nil :: t
  defrecordp :doc_nest, indent: 1 :: non_neg_integer, doc: :doc_nil :: t
  defrecordp :doc_break, str: " " :: binary
  defrecordp :doc_group, doc: :doc_nil :: t

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
  def concat(x, y), do: doc_cons(left: x, right: y)

  @doc """
  Concatenates a list of documents.
  """
  @spec concat([t]) :: doc_cons_t
  def concat(docs), do: folddoc(docs, concat(&1, &2))

  @doc """
  Nests document entity `x` positions deep. Nesting will be
  appended to the line breaks.

  ## Examples

      iex> doc = Inspect.Algebra.nest(Inspect.Algebra.concat(Inspect.Algebra.break, "6"), 5)
      iex> Inspect.Algebra.pretty(doc, 80)
      " 6"

  """
  @spec nest(t, non_neg_integer) :: doc_nest_t
  def nest(x, 0),                    do: x
  def nest(x, i) when is_integer(i), do: doc_nest(indent: i, doc: x)

  @doc %B"""
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

  @doc %B"""
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
  def group(d), do: doc_group(doc: d)

  @doc """
  Inserts a mandatory single space between two document entities.

  ## Examples

      iex> doc = Inspect.Algebra.space "Hughes", "Wadler"
      iex> Inspect.Algebra.pretty(doc, 80)
      "Hughes Wadler"

  """
  @spec space(t, t) :: doc_cons_t
  def space(x, y), do: concat(x, concat(" ", y))

  @doc %B"""
  Inserts a mandatory linebreak between two document entities.

  ## Examples

      iex> doc = Inspect.Algebra.line "Hughes", "Wadler"
      iex> Inspect.Algebra.pretty(doc, 80)
      "Hughes\nWadler"

  """
  @spec line(t, t) :: doc_cons_t
  def line(x, y), do: concat(x, concat(@newline, y))

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
  @spec folddoc([any], ((any, [t]) -> t)) :: t
  def folddoc([], _), do: empty
  def folddoc([doc], _), do: doc
  def folddoc([d|ds], f), do: f.(d, folddoc(ds, f))

  # Elixir conveniences

  @doc %B"""
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
    group concat [left, nest(doc, @nesting), right]
  end

  @doc %B"""
  Maps and glues a collection of items together using the given separator
  and surrounds them. A limit can be passed which, once reached, stops
  gluing and outputs "..." instead.

  ## Examples

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]", :infinity, integer_to_binary(&1))
      iex> Inspect.Algebra.pretty(doc, 5)
      "[1,\n 2,\n 3,\n 4,\n 5]"

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]", 3, integer_to_binary(&1))
      iex> Inspect.Algebra.pretty(doc, 20)
      "[1, 2, 3, ...]"

  """
  @spec surround_many(binary, [any], binary, integer | :infinity, (term -> t)) :: t
  def surround_many(left, [], right, _, _fun) do
    concat(left, right)
  end

  def surround_many(left, docs, right, limit, fun) do
    surround(left, surround_many(docs, limit, fun), right)
  end

  defp surround_many(_, 0, _fun) do
    "..."
  end

  defp surround_many([h], _limit, fun) do
    fun.(h)
  end

  defp surround_many([h|t], limit, fun) when is_list(t) do
    glue(
      concat(fun.(h), @surround_separator),
      surround_many(t, decrement(limit), fun)
    )
  end

  defp surround_many([h|t], _limit, fun) do
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

  # Records representing __simple__ documents, already on a fixed layout
  # Those are generalized by `sdoc` type.
  @type sdoc :: :s_nil | s_text_t | s_line_t
  defrecordp :s_text, str: "" :: binary, sdoc: :s_nil :: sdoc
  defrecordp :s_line, indent: 1 :: non_neg_integer, sdoc: :s_nil :: sdoc

  # Record representing the document mode to be rendered: flat or broken
  @typep mode :: :flat | :break

  @doc false
  @spec fits?(integer, [{ integer, mode, t }]) :: boolean
  def fits?(:infinity, _),                                  do: true # no pretty printing
  def fits?(w, _) when w < 0,                               do: false
  def fits?(_, []),                                         do: true
  def fits?(w, [{_, _, :doc_nil} | t]),                     do: fits?(w, t)
  def fits?(w, [{i, m, doc_cons(left: x, right: y)} | t]),  do: fits?(w, [{i, m, x} | [{i, m, y} | t]])
  def fits?(w, [{i, m, doc_nest(indent: j, doc: x)} | t]),  do: fits?(w, [{i + j, m, x} | t])
  def fits?(w, [{_, _, s} | t]) when is_binary(s),          do: fits?((w - byte_size s), t)
  def fits?(w, [{_, :flat, doc_break(str: s)} | t]),        do: fits?((w - byte_size s), t)
  def fits?(_, [{_, :break, doc_break(str: _)} | _]),       do: true
  def fits?(w, [{i, _, doc_group(doc: x)} | t]),            do: fits?(w, [{i, :flat, x} | t])

  @doc false
  @spec format(integer, integer, [{ integer, mode, t }]) :: atom | tuple
  def format(_, _, []),                                        do: :s_nil
  def format(w, k, [{_, _, :doc_nil} | t]),                    do: format(w, k, t)
  def format(w, k, [{i, m, doc_cons(left: x, right: y)} | t]), do: format(w, k, [{i, m, x} | [{i, m, y} | t]])
  def format(w, k, [{i, m, doc_nest(indent: j, doc: x)} | t]), do: format(w, k, [{i + j, m, x} | t])
  def format(w, k, [{_, _, s} | t]) when is_binary(s),         do: s_text(str: s, sdoc: format(w, (k + byte_size s), t))
  def format(w, k, [{i, m, doc_group(doc: x)} | t]),           do: format(w, k, [{i, m, x} | t])
  def format(w, k, [{_, :flat, doc_break(str: s)} | t]),       do: s_text(str: s, sdoc: format(w, (k + byte_size s), t))
  def format(w, k, [{i, :break, doc_break(str: s)} | t]) do
    k = k + byte_size(s)
    if fits?(w - k, t) do
      s_text(str: s, sdoc: format(w, k, t))
    else
      s_line(indent: i, sdoc: format(w, i, t))
    end
  end

  @doc false
  @spec render(sdoc) :: binary
  def render(sdoc) do
    iolist_to_binary do_render sdoc
  end

  @spec do_render(sdoc) :: [binary]
  defp do_render(:s_nil), do: [""]
  defp do_render(s_text(str: s, sdoc: d)), do: [s | do_render(d)]
  defp do_render(s_line(indent: i, sdoc: d)) do
    prefix = repeat " ", i
    [@newline | [prefix | do_render d]]
  end
end
