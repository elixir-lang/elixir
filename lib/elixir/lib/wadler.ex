defmodule Wadler do
  @moduledoc """
  Elixir implementation of the Wadler document algebra as described in
  ["Strictly Pretty" (2000) by Christian Lindig][0].

  The original Haskell implementation of the algorithm relies on lazy
  evaluation to unfold document groups on two alternatives:
  _flat_ (breaks as spaces) and _broken_ (breakes as newlines).
  Implementing the same logic on a strict language such as Elixir leads
  to an exponential growth of the possible documents, unless document
  groups are encoded explictly as _flat_ or _broken_. Those groups are
  then reduced to a simple document, where the layout is already decided.

  Custom pretty printers can be implemented using functions, exported
  from this module.

    [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

  """

  # some shortcut functions
  defp newline, do: "\n"
  defp strlen(s), do: String.length(s)
  defp default_nesting, do: 2

  defp repeat(_, 0), do: ""
  defp repeat(s, i), do: :lists.duplicate(i, s)

  # Records representing a __complex__ document
  @type doc :: :doc_nil | :doc_cons_t | :doc_text_t | :doc_nest_t | :doc_break_t | :doc_group_t
  defrecordp :doc_cons, :doc_cons, [left: :doc_nil, right: :doc_nil]
  defrecordp :doc_text, :doc_text, [str: ""]
  defrecordp :doc_nest, :doc_nest, [indent: 1, doc: :doc_nil]
  defrecordp :doc_break, :doc_break, [str: " "]
  defrecordp :doc_group, :doc_group, [doc: :doc_nil]

  # Functional interface to `doc` records
  @doc """
  Returns :doc_nil which is a document entity used to represent
  nothingness. Takes no arguments.

  ## Examples

      iex> Wadler.empty
      :doc_nil

  """
  @spec empty() :: :doc_nil
  def empty, do: :doc_nil

  @doc """
  Concatenates two document entities. Takes two arguments:
  left doc and right doc. Returns a DocCons doc

  ## Examples

      iex> doc = Wadler.concat Wadler.text("Tasteless"), Wadler.text("Artosis")
      iex> Wadler.pretty(doc, 80)
      "TastelessArtosis"

  """
  @spec concat(doc, doc) :: :doc_cons_t
  def concat(x, y), do: doc_cons(left: x, right: y)


  @doc """
  Nests document entity x positions deep. Nesting will be
  appended to the line breaks.

  ## Examples

      iex> doc = Wadler.nest(Wadler.concat(Wadler.break, Wadler.text("6")), 5)
      iex> Wadler.pretty(doc, 80)
      " 6"

  """
  @spec nest(non_neg_integer, doc) :: :doc_nest_t
  def nest(x, 0),                    do: x
  def nest(x, i) when is_integer(i), do: doc_nest(indent: i, doc: x)

  @doc """
  Document entity representation of a text.

  ## Examples

      iex> doc = Wadler.text "Hello, World!"
      iex> Wadler.pretty(doc, 80)
      "Hello, World!"

  """
  @spec text(binary) :: :doc_text_t
  def text(s) when is_binary(s), do: doc_text(str: s)

  @doc %B"""
  Document entity representating a break. This break can
  be rendered as a linebreak or as spaces, depending on the
  `mode` of the chosen layout or the provided separator.

  ## Examples

  Let's glue two docs together with a break and then render it:

    iex> doc = Wadler.glue(Wadler.text("a"), " ", Wadler.text("b"))
    iex> Wadler.pretty(doc, 80)
    "a b"

  Notice the break was represented as is, because we haven't reached
  a line limit. Once we do, it is replaced by a new line:

      iex> doc = Wadler.glue(Wadler.text(String.duplicate "a", 20), " ", Wadler.text("b"))
      iex> Wadler.pretty(doc, 10)
      "aaaaaaaaaaaaaaaaaaaa\nb"

  """
  @spec break(binary) :: :doc_break_t
  def break(s) when is_binary(s), do: doc_break(str: s)

  @spec break() :: :doc_break_t
  def break(), do: doc_break(str: " ")

  @doc """
  Inserts a break between two docs. See `break/1` for more info.
  """
  @spec glue(doc, doc) :: :doc_cons_t
  def glue(x, y), do: concat(x, concat(break, y))

  @doc """
  Inserts a break, passed as second argument, between two docs,
  the first and the third argument.
  """
  @spec glue(doc, binary, doc) :: :doc_cons_t
  def glue(x, g, y) when is_binary(g), do: concat(x, concat(break(g), y))

  @doc %B"""
  Returns a group containing the specified document.

  ## Examples

      iex> doc = Wadler.group(
      ...>   Wadler.concat(
      ...>     Wadler.group(
      ...>       Wadler.concat(
      ...>         Wadler.text("Hello,"),
      ...>         Wadler.concat(
      ...>           Wadler.break,
      ...>           Wadler.text("A")
      ...>         )
      ...>       )
      ...>     ),
      ...>     Wadler.concat(
      ...>       Wadler.break,
      ...>       Wadler.text("B")
      ...>     )
      ...> ))
      iex> Wadler.pretty(doc, 80)
      "Hello, A B"
      iex> Wadler.pretty(doc, 6)
      "Hello,\nA\nB"

  """
  @spec group(doc) :: :doc_group_t
  def group(d), do: doc_group(doc: d)

  # Helpers
  @doc """
  Inserts a mandatory single space between two document entities.

  ## Examples

      iex> doc = Wadler.space Wadler.text("Hughes"), Wadler.text("Wadler")
      iex> Wadler.pretty(doc, 80)
      "Hughes Wadler"

  """
  @spec space(doc, doc) :: :doc_cons_t
  def space(x, y), do: concat(x, concat(text(" "), y))

  @doc %B"""
  Inserts a mandatory linebreak between two document entities.

  ## Examples

      iex> doc = Wadler.line Wadler.text("Hughes"), Wadler.text("Wadler")
      iex> Wadler.pretty(doc, 80)
      "Hughes\nWadler"

  """
  @spec line(doc, doc) :: :doc_cons_t
  def line(x, y), do: glue(x, newline, y)

  @doc """
  Folds a list of document entities into a document entity
  using a function that is passed as the first argument.

  ## Examples

      iex> doc = [Wadler.text("A"), Wadler.text("B")]
      iex> doc = Wadler.folddoc(doc, fn(x,y) -> Wadler.concat(x, Wadler.concat(Wadler.text("!"), y)) end)
      iex> Wadler.pretty(doc, 80)
      "A!B"

  """
  @spec folddoc([doc], ((doc, [doc]) -> doc)) :: doc
  def folddoc([], _), do: empty
  def folddoc([doc], _), do: doc
  def folddoc([d|ds], f), do: f.(d, folddoc(ds, f))

  @doc """
  Folds a list of document entities with `space/2` function.

  ## Examples

      iex> doc = [Wadler.text("A"), Wadler.text("B")]
      iex> doc |> Wadler.spread |> Wadler.pretty(80)
      "A B"

  """
  @spec spread([doc]) :: doc
  def spread(docs), do: folddoc(docs, fn(d, x) -> space(d, x) end)

  @doc %B"""
  Folds a list of document entities with `line/2` function.

  ## Examples

      iex> doc = [Wadler.text("A"), Wadler.text("B")]
      iex> doc |> Wadler.stack |> Wadler.pretty(80)
      "A\nB"
  """
  @spec stack([doc]) :: doc
  def stack(docs), do: folddoc(docs, fn(d, x) -> line(d, x) end)

  @doc """
  Surrounds a document with characters.
  Puts the document between left and right enclosing and nests it.
  """
  @spec surround(binary, doc, binary, binary) :: doc
  @spec surround(binary, doc, binary) :: doc
  def surround(left, doc, right, sep // "") do
    glue(
      nest(glue(text(left), sep, doc), default_nesting), # remember that first line is not nested
      sep,
      text(right)
    )
  end

  # Records representing __simple__ documents, already on a fixed layout
  # Those are generalized by `sdoc` type.
  @type sdoc :: :s_nil | :s_text_t | :s_line_t
  defrecordp :s_text, :s_text, [str: "", sdoc: :s_nil]
  defrecordp :s_line, :s_line, [indent: 1, sdoc: :s_nil] # newline + spaces

  @doc """
  Renders a simple document into a binary
  """
  @spec render(sdoc) :: binary
  def render(sdoc) do
    iolist_to_binary do_render sdoc
  end

  @spec do_render(sdoc) :: [binary]
  defp do_render(:s_nil), do: [""]
  defp do_render(s_text(str: s, sdoc: d)), do: [s | do_render(d)]
  defp do_render(s_line(indent: i, sdoc: d)) do
    prefix = repeat " ", i
    [newline | [prefix | do_render d]]
  end

  @doc """
  The pretty printing function.
  Takes maximum width and document to print as its arguments and returns the string
  representation of the best layout for the document to fit in the given width.
  """
  @spec pretty(doc, non_neg_integer) :: binary
  def pretty(d, w) do
    sdoc = format w, 0, [{0, :flat, doc_group(doc: d)}]
    render(sdoc)
  end

  # Record representing the document mode to be rendered: __flat__ or __broken__
  @type mode :: :flat | :break

  # The fits? and format functions have to deal explicitly with the document modes
  @doc false
  @spec fits?(integer, [{ integer, mode, doc }]) :: boolean
  def fits?(:infinity, _),                                  do: true # no pretty printing
  def fits?(w, _) when w < 0,                               do: false
  def fits?(_, []),                                         do: true
  def fits?(w, [{_, _, :doc_nil} | t]),                     do: fits?(w, t)
  def fits?(w, [{i, m, doc_cons(left: x, right: y)} | t]),  do: fits?(w, [{i, m, x} | [{i, m, y} | t]])
  def fits?(w, [{i, m, doc_nest(indent: j, doc: x)} | t]),  do: fits?(w, [{i + j, m, x} | t])
  def fits?(w, [{_, _, doc_text(str: s)} | t]),             do: fits?((w - strlen s), t)
  def fits?(w, [{_, :flat, doc_break(str: s)} | t]),        do: fits?((w - strlen s), t)
  def fits?(_, [{_, :break, doc_break(str: _)} | _]),       do: true
  def fits?(w, [{i, _, doc_group(doc: x)} | t]),            do: fits?(w, [{i, :flat, x} | t])

  @doc false
  @spec format(integer, integer, [{ integer, mode, doc }]) :: atom | tuple
  def format(:infinity, k, [{i, _, doc_group(doc: x)} | t]),   do: format(:infinity, k, [{i, :flat, x} | t]) # no pretty printing
  def format(_, _, []),                                        do: :s_nil
  def format(w, k, [{_, _, :doc_nil} | t]),                    do: format(w, k, t)
  def format(w, k, [{i, m, doc_cons(left: x, right: y)} | t]), do: format(w, k, [{i, m, x} | [{i, m, y} | t]])
  def format(w, k, [{i, m, doc_nest(indent: j, doc: x)} | t]), do: format(w, k, [{i + j, m, x} | t])
  def format(w, k, [{_, _, doc_text(str: s)} | t]),            do: s_text(str: s, sdoc: format(w, (k + strlen s), t))
  def format(w, k, [{_, :flat, doc_break(str: s)} | t]),       do: s_text(str: s, sdoc: format(w, (k + strlen s), t))
  def format(w, _, [{i, :break, doc_break(str: _)} | t]),      do: s_line(indent: i, sdoc: format(w, i, t))
  def format(w, k, [{i, _, doc_group(doc: x)} | t]) do
    if fits? (w - k), [{i, :flat, x} | t] do
      format w, k, [{i, :flat, x} | t]
    else
      format w, k, [{i, :break, x} | t]
    end
  end
end
