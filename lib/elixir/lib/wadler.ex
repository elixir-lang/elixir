defmodule Wadler do
  @moduledoc """
  Pretty printing library inspired by Philip Wadler.

  Custom pretty printers can be implemented using
  functions, exported from this module.

  ## Examples

    iex> Wadler.text "foo"                      
    Wadler.TEXT[string: "foo"]
    
    iex> Wadler.group(Wadler.concat(Wadler.text("1"), Wadler.concat(Wadler.line, Wadler.text("2"))))
    Wadler.UNION[
      left: Wadler.CONCAT[
        left: Wadler.TEXT[string: "1"], 
        right: Wadler.CONCAT[left: Wadler.TEXT[string: " "], right: Wadler.TEXT[string: "2"]] 
      ], 
      right: Wadler.CONCAT[
        left: Wadler.TEXT[string: "1"], 
        right: Wadler.CONCAT[left: LINE, right: Wadler.TEXT[string: "2"]]
      ]
    ]

  """

  # Records to work with variant document representation
  # Those are generalized by `docentity` type.
  # Note that function that returns atom NIL is called `null`
  @type docentity :: TEXT.t | CONCAT.t | UNION.t | NEST.t | GLUE.t | NIL | LINE
  defrecord TEXT, string: ""
  defrecord CONCAT, left: nil, right: nil
  defrecord UNION, left: nil, right: nil
  defrecord NEST, indent: 1, rest: nil
  defrecord GLUE, string: "" 

  # Records that represent finalized entities in a document
  # Those are generalized by `docfactor` type.
  @type docfactor :: Text.t | Line.t | Nil
  defrecord Text, string: "", rest: nil
  defrecord Line, indent: 0,  rest: nil

  # Functional interface to `docentity` records
  @doc """
  Returns NIL which is a document entity used to represent
  nothingness. Takes no arguments.

  ## Examples

    iex> Wadler.null
    NIL

  """
  @spec null() :: NIL
  def null, do: NIL

  @doc """
  Concatenates two document entities. Takes two arguments:
  left docentity and right docentity. Returns CONCAT 
  docentity.

  ## Examples
    
    iex(1)> Wadler.concat Wadler.text("Tasteless"), Wadler.text("Artosis")
    Wadler.CONCAT[left: Wadler.TEXT[string: "Tasteless"], right: Wadler.TEXT[string: "Artosis"]]  

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    TastelessArtosis
    :ok

  """
  @spec concat(docentity, docentity) :: CONCAT.t
  def concat(x, y), do: CONCAT[left: x, right: y]

  @doc """
  Nests document entity x positions deep. Nesting will be
  appended to the line breaks.

  ## Examples

    iex(1)> Wadler.nest(5, Wadler.concat(Wadler.line, Wadler.text("6")))
    Wadler.NEST[indent: 5, rest: Wadler.CONCAT[left: LINE, right: Wadler.TEXT[string: "6"]]]

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    
         6
    :ok

  """
  @spec nest(non_neg_integer, docentity) :: NEST.t
  def nest(i, x) when is_integer(i), do: NEST[indent: i, rest: x]

  @doc """
  Document entity representation of a text.

  ## Examples 

    iex(1)> Wadler.text "Hello, World!"
    Wadler.TEXT[string: "Hello, World!"]

    iex(2)> IO.puts Wadler.pretty(80, v(9))
    Hello, World!
    :ok

  """
  @spec text(binary) :: TEXT.t
  def text(s) when is_binary(s), do: TEXT[string: s]
  
  @doc """
  Document entity representation of a line break. It's
  the place where nesting spaces get appended. `group`
  of a document entity will transform lines breaks in
  single space characters and `group1` of a document
  will transform line breaks into NIL.

  ## Examples

    iex(1)> Wadler.line
    LINE
    
    iex(2)> Wadler.group v(1) # flatten is called here
    Wadler.UNION[left: Wadler.TEXT[string: " "], right: LINE]

    iex(3)> Wadler.group1 v(1) # shrink is called here
    Wadler.UNION[left: NIL, right: LINE]

    iex(4)> IO.puts Wadler.pretty(80, v(1))


    :ok

  """
  @spec line() :: LINE
  def line, do: LINE

  @doc """
  Returns a single space character of glue text.

  Glue text is only visible in multiline layouts.
  Call to both `group` and `group1` of a document
  destroys glue.

  ## Examples

    iex(1)> Wadler.glue
    Wadler.GLUE[string: " "]

    iex(2)> Wadler.group v(1)
    Wadler.UNION[left: NIL, right: Wadler.GLUE[string: " "]]

    iex(3)> Wadler.group1 v(1)
    Wadler.UNION[left: NIL, right: Wadler.GLUE[string: " "]]

    iex(4)> IO.puts Wadler.pretty(80, v(1)) # There is a space down there!
     
    :ok

  """
  @spec glue() :: GLUE.t
  def glue(), do: GLUE[string: " "]

  @doc """
  Makes glue text from a binary.

  Glue text is only visible in multiline layouts.
  Call to both `group` and `group1` of a document
  destroys glue.

  ## Examples

    iex(1)> Wadler.glue "foo"
    Wadler.GLUE[string: "foo"]

    iex(2)> Wadler.group v(1)
    Wadler.UNION[left: NIL, right: Wadler.GLUE[string: "foo"]]

    iex(3)> Wadler.group1 v(1)
    Wadler.UNION[left: NIL, right: Wadler.GLUE[string: "foo"]]

    iex(4)> IO.puts Wadler.pretty 80, v(1)
    foo
    :ok

    iex(5)> IO.puts Wadler.pretty 80, v(2)

    :ok

  """
  @spec glue(binary) :: GLUE.t
  def glue(x) when is_binary(x), do: GLUE[string: x]

  @doc"""
  Inserts single space glue between two docentities.
  """
  @spec glue(docentity, docentity) :: CONCAT.t
  def glue(x, y), do: concat(x, concat(glue, y))

  @doc"""
  Inserts glue, passed as second argument between
  docentity, passed as the first argument and
  docentity, passed as the third argument.
  """
  @spec glue(docentity, binary, docentity) :: CONCAT.t
  def glue(x, g, y) when is_binary(g), do: concat(x, concat(glue(g), y))

  @doc """
  Returns a union of a document entitiy in
  which left document entity is flattened 
  version of the initial document and right
  document entity is the initial document
  entity.

  ## Examples

    iex(1)> Wadler.group(Wadler.concat(Wadler.group(
    ...(1)>                                         Wadler.concat(Wadler.text("Hello,"),
    ...(1)>                                                       Wadler.concat(Wadler.line,
    ...(1)>                                                                     Wadler.text("A"))
    ...(1)>                                         )),
    ...(1)>                            Wadler.concat(Wadler.line,
    ...(1)>                                          Wadler.text("B"))
    ...(1)> )) # Output is prettified for readability.
    UNION[
      left:CONCAT[
        left:CONCAT[
          left:TEXT[string: "Hello,"]
          right:CONCAT[
            left:TEXT[string: " "]
            right:TEXT[string: "A"]]]
        right:CONCAT[
          left:TEXT[string: " "]
          right:TEXT[string: "B"]]]
      right:CONCAT[
        left:UNION[
          left:CONCAT[
            left:TEXT[string: "Hello,"]
            right:CONCAT[
              left:TEXT[string: " "]
              right:TEXT[string: "A"]]]
          right:CONCAT[
            left:TEXT[string: "Hello,"]
            right:CONCAT[
              left:LINE
              right:TEXT[string: "A"]]]]
        right:CONCAT[
          left:LINE
          right:TEXT[string: "B"]]]]

    iex(2)> IO.puts Wadler.pretty(80, v(1))
    Hello, A B
    :ok

    iex(3)> IO.puts Wadler.pretty(6, v(1)) 
    Hello,
    A
    B
    :ok
  """
  @spec group(docentity) :: UNION.t
  def group(x),  do: UNION[left: flatten(x), right: x]

  @doc """
  Does the same thing as `group` but collapses
  line breaks into NILs. Can be handy for
  pretty printing terms, representations of
  which have natural separators (such as lists).

  ## Examples

    iex(1)> Wadler.group1(Wadler.concat(Wadler.group1(
    ...(1)>                                           Wadler.concat(Wadler.text("Hello,"),
    ...(1)>                                                         Wadler.concat(Wadler.line,
    ...(1)>                                                                       Wadler.text("A"))
    ...(1)>                                           )),
    ...(1)>                              Wadler.concat(Wadler.line,
    ...(1)>                                            Wadler.text("B"))
    ...(1)> )) # Output is prettified for readability
    UNION[
      left:CONCAT[
        left:CONCAT[
          left:TEXT[string: "Hello,"]
          right:CONCAT[
            left:NIL
            right:TEXT[string: "A"]]]
        right:CONCAT[
          left:NIL
          right:TEXT[string: "B"]]]
      right:CONCAT[
        left:UNION[
          left:CONCAT[
            left:TEXT[string: "Hello,"]
            right:CONCAT[
              left:NIL
              right:TEXT[string: "A"]]]
          right:CONCAT[
            left:TEXT[string: "Hello,"]
            right:CONCAT[
              left:LINE
              right:TEXT[string: "A"]]]]
        right:CONCAT[
          left:LINE
          right:TEXT[string: "B"]]]]
      
  """
  @spec group1(docentity) :: UNION.t
  def group1(x), do: UNION[left: shrink(x),  right: x]

  # Helpers
  @doc """
  Inserts single space between two document entities.

  ## Examples
  
    iex(1)> Wadler.space Wadler.text("Hughes"), Wadler.text("Wadler")
    Wadler.CONCAT[
      left: Wadler.TEXT[string: "Hughes"], 
      right: Wadler.CONCAT[left: Wadler.TEXT[string: " "], right: Wadler.TEXT[string: "Wadler"]]
    ] 

  """
  @spec space(docentity, docentity) :: CONCAT.t
  def space(x, y), do: concat(x, concat(text(" "), y))
  
  @doc """
  Inserts single line between two document entities.

  ## Examples

    iex(1)> Wadler.line Wadler.text("Hughes"), Wadler.text("Wadler")
    Wadler.CONCAT[
      left: Wadler.TEXT[string: "Hughes"], 
      right: Wadler.CONCAT[left: LINE, right: Wadler.TEXT[string: "Wadler"]]
    ]

  """
  @spec line(docentity, docentity) :: CONCAT.t
  def line(x, y), do: concat(x, concat(line, y))

  @doc """
  Produces document entity where separator
  is a union of space and line.

  ## Examples
    iex(1)> Wadler.sn Wadler.text("Hughes"), Wadler.text("Wadler")        
    Wadler.CONCAT[
      left: Wadler.TEXT[string: "Hughes"], 
      right: Wadler.CONCAT[
        left: Wadler.UNION[
          left: Wadler.TEXT[string: " "], 
          right: LINE], 
        right: Wadler.TEXT[string: "Wadler"]
      ]
    ]

  
  """
  @spec sn(docentity, docentity) :: CONCAT.t
  def sn(x, y), do: concat(x, concat(UNION[left: text(" "), right: line], y))

  @doc """
  Fold a list of document entities into a document entity
  using a function that is passed as the first argument.

  ## Example

    iex(1)> [Wadler.text("A"), Wadler.text("B")] 
    [Wadler.TEXT[string: "A"],Wadler.TEXT[string: "B"]]
    
    iex(2)> Wadler.folddoc(fn(x,y) -> Wadler.concat(x, Wadler.concat(Wadler.text("!"), y)) end, v(1))
    Wadler.CONCAT[
      left: Wadler.TEXT[string: "A"], 
      right: Wadler.CONCAT[
        left: Wadler.TEXT[string: "!"], 
        right: Wadler.TEXT[string: "B"]
      ]
    ]
    
  """
  @spec folddoc( ((docentity, [docentity]) -> docentity), [docentity]) :: docentity
  def folddoc(_, []), do: null
  def folddoc(_, [doc]), do: doc
  def folddoc(f, [d|ds]), do: f.(d, folddoc(f, ds))

  @doc """
  Fold a list of document entities with space/2 function.
  """
  @spec spread(docentity) :: docentity
  def spread(doc), do: folddoc(fn(x, d) -> space(x,d) end, doc)
  @spec stack(docentity) :: docentity

  @doc """
  Fold a list of document entities with line/2 function.
  """
  def stack(doc),  do: folddoc(fn(x, d) -> line(x,d) end, doc)

  @doc """
  Collapse a list of documents into a reasonably formatted document.
  """
  @spec fill([docentity]) :: docentity
  def fill([]), do: NIL
  def fill([doc]), do: doc
  def fill([x|[y|docs]]) do
    UNION[left:  space(flatten(x), fill( [flatten(y)|docs] )),
          right: line(x, fill( [y|docs] ))]
  end
  
  @doc """
  Takes three arguments: left bracket, document
  and right bracket.
  Puts the document between the brackets and nests
  it using `group`.
  """
  @spec bracket(binary, docentity, binary) :: docentity
  def bracket(bracketl, doc, bracketr) do
    group(
      concat(
             text(bracketl), 
             concat(
                    nest(2, concat(line, doc)),
                    concat(line, text(bracketr))
             )
      )
    )
  end  
  
  @doc"""
  The pretty printing functoion.
  Takes maximum width and document to print as
  its arguments and returns the string
  representation of the best layout for the
  document to fit in the given width.
  """
  @spec pretty(non_neg_integer, docentity) :: binary
  def pretty(width, document), do: layout best(width, 0, document)

  @doc"""
  Exported for the purpose of tests. You shouldn't
  use this function.
  """
  @spec factor(non_neg_integer, docentity) :: docfactor
  def factor(width, document), do: best width, 0, document
  
  ## Private functions

  # Flatten variant representation.
  # Non-terminals
  defp flatten(UNION[left: x, right: _]),  do: flatten x
  defp flatten(CONCAT[left: x, right: y]), do: CONCAT[left: flatten(x), right: flatten(y)]
  defp flatten(NEST[indent: i, rest: x]),  do: NEST[indent: i, rest: flatten(x)]
  # Terminals
  defp flatten(LINE),                      do: TEXT[string: " "]
  defp flatten(GLUE[]),                    do: NIL
  defp flatten(NIL),                       do: NIL
  defp flatten(x = TEXT[]),                do: x

  # Shrink is flatten version that replaces lines with NIL.
  # Non-terminals
  defp shrink(UNION[left: x, right: _]),   do: shrink x
  defp shrink(CONCAT[left: x, right: y]),  do: CONCAT[left: shrink(x), right: shrink(y)]
  defp shrink(NEST[indent: i, rest: x]),   do: NEST[indent: i, rest: shrink(x)]
  # Terminals
  defp shrink(LINE),                       do: NIL
  defp shrink(GLUE[]),                     do: NIL
  # Other terminals are same as with flatten
  defp shrink(x),                          do: flatten(x)

  # Laying out finalized document
  defp layout(Nil), do: ""
  defp layout(Text[string: s, rest: x]), do: s <> layout(x)
  defp layout(Line[indent: i, rest: x]), do: "\n" <> copy(" ", i) <> layout(x)

  defp copy(_, 0), do: ""
  defp copy(binary, i), do: String.duplicate binary, i


  # Choosing best layout
  defp best(width, start_pos, document), do: dobest width, start_pos, [{0,document}]

  # Best layout of \varempty is Nil
  defp dobest(_, _, []), do: Nil
  # Ignore NIL
  defp dobest(w, k, [{_,NIL}|z]), do: dobest w,k,z
  # Expand CONCAT into two candidates
  defp dobest(w, k, [{i,CONCAT[left: x, right: y]}|z]) do 
    dobest w,k,[{i,x}|[{i,y}|z]]
  end
  # Get indentation information from NEST and move on
  defp dobest(w, k, [{i,NEST[indent: j, rest: x]}|z]) do 
    dobest w,k,[{i+j,x}|z]
  end
  # Factor out TEXT or GLUE and move caret accordingly
  defp dobest(w, k, [{_,TEXT[string: s]}|z]) do 
    Text[string: s, rest: dobest w, k+String.length(s), z]
  end
  defp dobest(w, k, [{_,GLUE[string: s]}|z]) do
    Text[string: s, rest: dobest w, k+String.length(s), z]
  end
  # Factor out LINE and make the indentation be initial caret position on the new line
  defp dobest(w, _, [{i,LINE}|z]) do
    Line[indent: i, rest: dobest w, i, z]
  end
  # Choose better alternative from UNION
  defp dobest(w, k, [{i,UNION[left: x, right: y]}|z]) do
    better w, k, dobest(w,k,[{i,x}|z]), dobest(w,k,[{i,y}|z])
  end

  defp better(w, k, x, y), do: if fits?(w-k, x), do: x, else: y

  defp fits?(delta, _) when delta<0,  do: false
  defp fits?(_____, Nil),             do: true
  defp fits?(_____, Line[]),          do: true 
  defp fits?(delta, Text[string: s,
                           rest: x]), do: fits? delta - String.length(s), x

end
