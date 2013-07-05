defmodule Wadler do
  @moduledoc """
  Elixir implementation of the Wadler document algebra as described in 
  "Strictly Pretty" (2000) by Christian Lindig
  http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
   
  The original Haskell implementation of the algorithm relies on lazy 
  evaluation to unfold document groups on two alternatives:
  _flat_ (breaks as spaces) and _broken_ (breakes as newlines).
  Implementing the same logic on a strict language such as Elixir leads
  to an exponential growth of the possible documents, unless document
  groups are encoded explictly as _flat_ or _broken_. Those groups are
  then reduced to a simple document, where the layout is already decided.

  Custom pretty printers can be implemented using
  functions, exported from this module.

  ## Examples

    iex> Wadler.text "foo"                      
    Wadler.doc_text(str: "foo")
    
    iex> doc = Wadler.group(Wadler.concat(Wadler.text("1"), Wadler.concat(Wadler.break, Wadler.text("2"))))
    Wadler.doc_group(
      doc: Wadler.doc_cons(
        left: Wadler.doc_text(str: "1"), 
        right: Wadler.doc_cons(
          left: Wadler.doc_break(str: " "), 
          right: Wadler.doc_text(str: "2")
        )
      )
    )

    iex> sdoc = Wadler.format w, 0, [{0, :flat, doc})
    Wadler.SText[
      str: "1", 
      sdoc: Wadler.SText[
        str: " ", 
        sdoc: Wadler.SText[
          str: "2",
          sdoc: :s_nil
        )
      )
    )


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
    
    iex(1)> Wadler.concat Wadler.text("Tasteless"), Wadler.text("Artosis")
    Wadler.doc_cons(left: Wadler.doc_text(str: "Tasteless"), right: Wadler.doc_text(str: "Artosis"))  

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    TastelessArtosis
    :ok

  """
  @spec concat(doc, doc) :: :doc_cons_t
  def concat(x, y), do: doc_cons(left: x, right: y)
  

  @doc """
  Nests document entity x positions deep. Nesting will be
  appended to the line breaks.

  ## Examples

    iex(1)> Wadler.nest(5, Wadler.concat(Wadler.break, Wadler.text("6")))
    Wadler.doc_nest(indent: 5, doc: Wadler.doc_cons(left: Wadler.doc_break(str: " "), right: Wadler.doc_text(str: "6")))

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    
         6
    :ok

  """
  @spec nest(non_neg_integer, doc) :: :doc_nest_t
  def nest(0, x),                    do: x
  def nest(i, x) when is_integer(i), do: doc_nest(indent: i, doc: x)

  @doc """
  Document entity representation of a text.

  ## Examples 

    iex(1)> Wadler.text "Hello, World!"
    Wadler.doc_text(str: "Hello, World!")

    iex(2)> IO.puts Wadler.pretty(80, v(9))
    Hello, World!
    :ok

  """
  @spec text(binary) :: :doc_text_t
  def text(s) when is_binary(s), do: doc_text(str: s)

  
  @doc """
  Document entity representating a break. This break can
  be rendered as a linebreak or as spaces, depending on the
  `mode` of the chosen layout or the provided separator.
  
  ## Examples

    iex(2)> Wadler.break "hello"
    Wadler.doc_break(str: "hello")
   
    iex(3)> Wadler.break        
    Wadler.doc_break(str: " ")

    iex(4)> Wadler.concat(
              Wadler.concat(
                Wadler.text("a"), Wadler.break), Wadler.text("b"))

    iex(5)> Wadler.render Wadler.format w, 0, [{0, :flat, v(4)})
    "a b"

    iex(6)> Wadler.render Wadler.format w, 0, [{0, :break, v(4)})
    "a\nb"

  """
  @spec break(binary) :: :doc_break_t
  def break(s) when is_binary(s), do: doc_break(str: s)

  @spec break() :: :doc_break_t
  def break(), do: doc_break(str: " ")

  @doc"""
  Inserts a break between two docs.
  """
  @spec glue(doc, doc) :: :doc_cons_t
  def glue(x, y), do: concat(x, concat(break, y))

  @doc"""
  Inserts a break, passed as second argument, between two docs,
  the first and the third argument.
  """
  @spec glue(doc, binary, doc) :: :doc_cons_t
  def glue(x, g, y) when is_binary(g), do: concat(x, concat(break(g), y))

  @doc """
  Returns a group containing the specified document.
  ## Examples

    iex(1)> Wadler.group(
    ...(1)>   Wadler.concat(
    ...(1)>     Wadler.group(
    ...(1)>       Wadler.concat(
    ...(1)>         Wadler.text("Hello,"),
    ...(1)>         Wadler.concat(
    ...(1)>           Wadler.break,
    ...(1)>           Wadler.text("A")
    ...(1)>         )
    ...(1)>       )
    ...(1)>     ),
    ...(1)>     Wadler.concat(
    ...(1)>       Wadler.break,
    ...(1)>       Wadler.text("B")
    ...(1)>     )
    ...(1)> )) # Output is prettified for readability.
    Wadler.doc_group(
      doc: Wadler.doc_cons(
        left:  Wadler.doc_group(
          doc: Wadler.doc_cons(
            left:  Wadler.doc_text(str: "Hello,"), 
            right: Wadler.doc_cons(
              left:  Wadler.doc_break(str: " "), 
              right: Wadler.doc_text(str: "A")
            )
          )
        ), 
        right: Wadler.doc_cons(
          left:  Wadler.doc_break(str: " "), 
          right: Wadler.doc_text(str: "B")
        )
      )
    )

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    Hello, A B
    :ok

    iex(3)> IO.puts Wadler.pretty(6, v(1)) 
    Hello,
    A
    B
    :ok
  """
  @spec group(doc) :: :doc_group_t
  def group(d), do: doc_group(doc: d)

  # Helpers
  @doc """
  Inserts a mandatory single space between two document entities.

  ## Examples
  
    iex(1)> Wadler.space Wadler.text("Hughes"), Wadler.text("Wadler")
    Wadler.doc_cons(
      left:  Wadler.doc_text(str: "Hughes"), 
      right: Wadler.doc_cons(
        left:  Wadler.doc_text(str: " "),
        right: Wadler.doc_text(str: "Wadler")
      )
    ) 

  """
  @spec space(doc, doc) :: :doc_cons_t
  def space(x, y), do: concat(x, concat(text(" "), y))
  
  @doc """
  Inserts a mandatory linebreak between two document entities.

  ## Examples

    iex(1)> Wadler.line Wadler.text("Hughes"), Wadler.text("Wadler")
    Wadler.doc_cons(
      left:  Wadler.doc_text(str: "Hughes"), 
      right: Wadler.doc_cons(
        left:  Wadler.doc_text(str: "\n"),
        right: Wadler.doc_text(str: "Wadler")
      )
    )
  """
  @spec line(doc, doc) :: :doc_cons_t
  def line(x, y), do: glue(x, newline, y)

  @doc """
  Folds a list of document entities into a document entity
  using a function that is passed as the first argument.

  ## Example

    iex(1)> [Wadler.text("A"), Wadler.text("B")) 
    [Wadler.doc_text(string: "A"), Wadler.doc_text(string: "B"))
    
    iex(2)> Wadler.folddoc(fn(x,y) -> Wadler.concat(x, Wadler.concat(Wadler.text("!"), y)) end, v(1))
    Wadler.CONCAT[
      left: Wadler.doc_text(str: "A"), 
      right: Wadler.doc_cons(
        left: Wadler.doc_text(string: "!"), 
        right: Wadler.doc_text(string: "B")
      )
    )
    
  """
  @spec folddoc( ((doc, [doc]) -> doc), [doc]) :: doc 
  def folddoc(_, []), do: empty
  def folddoc(_, [doc]), do: doc
  def folddoc(f, [d|ds]), do: f.(d, folddoc(f, ds))

  @doc """
  Folds a list of document entities with space/2 function.
  """
  @spec spread(doc) :: doc
  def spread(doc), do: folddoc(fn(x, d) -> space(x, d) end, doc)

  @doc """
  Folds a list of document entities with line/2 function.
  """
  @spec stack(doc) :: doc
  def stack(doc),  do: folddoc(fn(x, d) -> line(x, d) end, doc)

  @doc """
  Surrounds a document with characters.
  Puts the document between left and right enclosing and nests it.
  """
  @spec surround(binary, doc, binary, binary) :: doc
  @spec surround(binary, doc, binary) :: doc
  def surround(left, doc, right, sep//"") do
    glue(
      nest(default_nesting, 
        glue(text(left), sep, doc)), # remember that first line is not nested
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
  @spec pretty(non_neg_integer, doc) :: binary
  def pretty(w, d) do
    sdoc = format w, 0, [{0, :flat, doc_group(doc: d)}]
    render(sdoc)
  end

  # Record representing the document mode to be rendered: __flat__ or __broken__
  @type mode :: :flat | :break

  # the fits? and format methods have to deal explicitly with the document modes
  @spec fits?(integer, [{ integer, mode, doc }]) :: boolean
  defp fits?(:infinity, _),                                  do: true # no pretty printing
  defp fits?(w, _) when w < 0,                               do: false
  defp fits?(_, []),                                         do: true
  defp fits?(w, [{_, _, :doc_nil} | t]),                     do: fits?(w, t)
  defp fits?(w, [{i, m, doc_cons(left: x, right: y)} | t]),  do: fits?(w, [{i, m, x} | [{i, m, y} | t]])
  defp fits?(w, [{i, m, doc_nest(indent: j, doc: x)} | t]),  do: fits?(w, [{i + j, m, x} | t])
  defp fits?(w, [{_, _, doc_text(str: s)} | t]),             do: fits?((w - strlen s), t)
  defp fits?(w, [{_, :flat, doc_break(str: s)} | t]),        do: fits?((w - strlen s), t)
  defp fits?(_, [{_, :break, doc_break(str: _)} | _]),       do: true
  defp fits?(w, [{i, _, doc_group(doc: x)} | t]),            do: fits?(w, [{i, :flat, x} | t])

  @spec format(integer, integer, [{ integer, mode, doc }]) :: boolean
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
