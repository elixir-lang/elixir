defmodule Wadler do
  @moduledoc """
  Pretty printing library inspired by Philip Wadler.

  Custom pretty printers can be implemented using
  functions, exported from this module.

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

  ## Examples

    iex> Wadler.text "foo"                      
    Wadler.DocText[str: "foo"]
    
    iex> doc = Wadler.group(Wadler.concat(Wadler.text("1"), Wadler.concat(Wadler.break, Wadler.text("2"))))
    Wadler.DocGroup[
      doc: Wadler.DocCons[
        left: Wadler.DocText[str: "1"], 
        right: Wadler.DocCons[
          left: Wadler.DocBreak[str: " "], 
          right: Wadler.DocText[str: "2"]
        ]
      ]
    ]

    iex> sdoc = Wadler.format w, 0, [{0, Wadler.Flat, doc}]
    Wadler.SText[
      str: "1", 
      sdoc: Wadler.SText[
        str: " ", 
        sdoc: Wadler.SText[
          str: "2",
          sdoc: SNil
        ]
      ]
    ]


  """

  # some shortcut functions
  defp newline, do: "\n"
  defp strlen(s), do: String.length(s)
  defp default_nesting, do: 2

  defp repeat(_, 0), do: ""
  defp repeat(s, i), do: String.duplicate s, i

  # Records representing a __complex__ document
  @type doc :: DocNil | DocCons.t | DocText.t | DocNest.t | DocBreak.t | DocGroup.t
  defrecord DocCons, left: DocNil, right: DocNil
  defrecord DocText, str: ""
  defrecord DocNest, indent: 1, doc: DocNil
  defrecord DocBreak, str: " "
  defrecord DocGroup, doc: DocNil

  # Functional interface to `doc` records
  @doc """
  Returns DocNil which is a document entity used to represent
  nothingness. Takes no arguments.

  ## Examples

    iex> Wadler.empty
    DocNil

  """
  @spec empty() :: DocNil
  def empty, do: DocNil

  @doc """
  Concatenates two document entities. Takes two arguments:
  left doc and right doc. Returns a DocCons doc

  ## Examples
    
    iex(1)> Wadler.concat Wadler.text("Tasteless"), Wadler.text("Artosis")
    Wadler.DocCons[left: Wadler.DocText[str: "Tasteless"], right: Wadler.DocText[str: "Artosis"]]  

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    TastelessArtosis
    :ok

  """
  @spec concat(doc, doc) :: DocCons.t
  def concat(x, y), do: DocCons[left: x, right: y]
  

  @doc """
  Nests document entity x positions deep. Nesting will be
  appended to the line breaks.

  ## Examples

    iex(1)> Wadler.nest(5, Wadler.concat(Wadler.break, Wadler.text("6")))
    Wadler.DocNest[indent: 5, doc: Wadler.DocCons[left: Wadler.DocBreak, right: Wadler.DocText[str: "6"]]]

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    
         6
    :ok

  """
  @spec nest(non_neg_integer, doc) :: DocNest.t
  def nest(0, x),                    do: x
  def nest(i, x) when is_integer(i), do: DocNest[indent: i, doc: x]

  @doc """
  Document entity representation of a text.

  ## Examples 

    iex(1)> Wadler.text "Hello, World!"
    Wadler.DocText[str: "Hello, World!"]

    iex(2)> IO.puts Wadler.pretty(80, v(9))
    Hello, World!
    :ok

  """
  @spec text(binary) :: DocText.t
  def text(s) when is_binary(s), do: DocText[str: s]

  
  @doc """
  Document entity representating a break. This break can
  be rendered as a linebreak or as spaces, depending on the
  `mode` of the chosen layout or the provided separator.
  
  ## Examples

    iex(2)> Wadler.break "hello"
    Wadler.DocBreak[str: "hello"]
   
    iex(3)> Wadler.break        
    Wadler.DocBreak[str: " "]

    iex(4)> Wadler.concat(
              Wadler.concat(
                Wadler.text("a"), Wadler.break), Wadler.text("b"))

    iex(5)> Wadler.render Wadler.format w, 0, [{0, Wadler.Flat, v(4)}]
    "a b"

    iex(6)> Wadler.render Wadler.format w, 0, [{0, Wadler.Break, v(4)}]
    "a\nb"

  """
  @spec break(binary) :: DocBreak.t
  def break(s) when is_binary(s), do: DocBreak[str: s]

  @spec break() :: DocBreak.t
  def break(), do: DocBreak[str: " "]

  @doc"""
  Inserts a break between two docs.
  """
  @spec glue(doc, doc) :: DocCons.t
  def glue(x, y), do: concat(x, concat(break, y))

  @doc"""
  Inserts a break, passed as second argument, between two docs,
  the first and the third argument.
  """
  @spec glue(doc, binary, doc) :: DocCons.t
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
    Wadler.DocGroup[
      doc: Wadler.DocCons[
        left:  Wadler.DocGroup[
          doc: Wadler.DocCons[
            left:  Wadler.DocText[str: "Hello,"], 
            right: Wadler.DocCons[
              left:  Wadler.DocBreak[str: " "], 
              right: Wadler.DocText[str: "A"]
            ]
          ]
        ], 
        right: Wadler.DocCons[
          left:  Wadler.DocBreak[str: " "], 
          right: Wadler.DocText[str: "B"]
        ]
      ]
    ]

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    Hello, A B
    :ok

    iex(3)> IO.puts Wadler.pretty(6, v(1)) 
    Hello,
    A
    B
    :ok
  """
  @spec group(doc) :: DocGroup.t
  def group(d), do: DocGroup[doc: d]

  # Helpers
  @doc """
  Inserts a mandatory single space between two document entities.

  ## Examples
  
    iex(1)> Wadler.space Wadler.text("Hughes"), Wadler.text("Wadler")
    Wadler.DocCons[
      left:  Wadler.DocText[str: "Hughes"], 
      right: Wadler.DocCons[
        left:  Wadler.DocText[str: " "],
        right: Wadler.DocText[str: "Wadler"]
      ]
    ] 

  """
  @spec space(doc, doc) :: DocCons.t
  def space(x, y), do: concat(x, concat(text(" "), y))
  
  @doc """
  Inserts a mandatory linebreak between two document entities.

  ## Examples

    iex(1)> Wadler.line Wadler.text("Hughes"), Wadler.text("Wadler")
    Wadler.DocCons[
      left:  Wadler.DocText[str: "Hughes"], 
      right: Wadler.DocCons[
        left:  Wadler.DocText[str: "\n"],
        right: Wadler.DocText[str: "Wadler"]
      ]
    ]
  """
  @spec line(doc, doc) :: DocCons.t
  def line(x, y), do: glue(x, newline, y)

  @doc """
  Folds a list of document entities into a document entity
  using a function that is passed as the first argument.

  ## Example

    iex(1)> [Wadler.text("A"), Wadler.text("B")] 
    [Wadler.DocText[string: "A"], Wadler.DocText[string: "B"]]
    
    iex(2)> Wadler.folddoc(fn(x,y) -> Wadler.concat(x, Wadler.concat(Wadler.text("!"), y)) end, v(1))
    Wadler.CONCAT[
      left: Wadler.DocText[str: "A"], 
      right: Wadler.DocCons[
        left: Wadler.DocText[string: "!"], 
        right: Wadler.DocText[string: "B"]
      ]
    ]
    
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
      text(left), 
      sep,
      glue(
        nest(default_nesting, doc),
        sep,
        text(right)
      )
    )
  end  

  # Records representing __simple__ documents, already on a fixed layout
  # Those are generalized by `sdoc` type.
  @type sdoc :: SNil | SText.t | SLine.t
  defrecord SText, str: "", sdoc: SNil
  defrecord SLine, indent: 1, sdoc: SNil # newline + spaces

  @doc """
  Renders a simple document into a binary
  """
  @spec render(sdoc) :: binary
  def render(sdoc) do
    iolist_to_binary do_render sdoc
  end
  
  @spec do_render(sdoc) :: [binary]
  defp do_render(SNil), do: [""]
  defp do_render(SText[str: s, sdoc: d]), do: [s | do_render(d)]
  defp do_render(SLine[indent: i, sdoc: d]) do
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
    sdoc = format w, 0, [{0, Flat, DocGroup[doc: d]}]
    render(sdoc)
  end

  # Record representing the document mode to be rendered: __flat__ or __broken__
  @type mode :: Flat | Break

  # the fits? and format methods have to deal explicitly with the document modes
  @spec fits?(integer, [{ integer, mode, doc }]) :: boolean
  defp fits?(w, _) when w < 0,                              do: false
  defp fits?(_, []),                                        do: true
  defp fits?(w, [{_, _, DocNil} | t]),                      do: fits? w, t
  defp fits?(w, [{i, m, DocCons[left: x, right: y]} | t]),  do: fits? w, [{i, m, x} | [{i, m, y} | t]]
  defp fits?(w, [{i, m, DocNest[indent: j, doc: x]} | t]),  do: fits? w, [{i + j, m, x} | t]
  defp fits?(w, [{_, _, DocText[str: s]} | t]),             do: fits? (w - strlen s), t
  defp fits?(w, [{_, Flat, DocBreak[str: s]} | t]),         do: fits? (w - strlen s), t
  defp fits?(_, [{_, Break, DocBreak[str: _]} | _]),        do: true # impossible (but why?)
  defp fits?(w, [{i, _, DocGroup[doc: x]} | t]),            do: fits? w, [{i, Flat, x} | t]

  @spec format(integer, integer, [{ integer, mode, doc }]) :: boolean
  def format(_, _, []),                                       do: SNil
  def format(w, k, [{_, _, DocNil} | t]),                     do: format w, k, t
  def format(w, k, [{i, m, DocCons[left: x, right: y]} | t]), do: format w, k, [{i, m, x} | [{i, m, y} | t]]
  def format(w, k, [{i, m, DocNest[indent: j, doc: x]} | t]), do: format w, k, [{i + j, m, x} | t]
  def format(w, k, [{_, _, DocText[str: s]} | t]),            do: SText[str: s, sdoc: format w, (k + strlen s), t]
  def format(w, k, [{_, Flat, DocBreak[str: s]} | t]),        do: SText[str: s, sdoc: format w, (k + strlen s), t]
  def format(w, _, [{i, Break, DocBreak[str: _]} | t]),       do: SLine[indent: i, sdoc: format w, i, t]
  def format(w, k, [{i, _, DocGroup[doc: x]} | t]) do
    if fits? (w - k), [{i, Flat, x} | t] do
      format w, k, [{i, Flat, x} | t]
    else
      format w, k, [{i, Break, x} | t]
    end
  end
end
