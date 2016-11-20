defmodule Inspect.Opts do
  @moduledoc """
  Defines the Inspect.Opts used by the Inspect protocol.

  The following fields are available:

    * `:structs` - when `false`, structs are not formatted by the inspect
      protocol, they are instead printed as maps, defaults to `true`.

    * `:binaries` - when `:as_strings` all binaries will be printed as strings,
      non-printable bytes will be escaped.

      When `:as_binaries` all binaries will be printed in bit syntax.

      When the default `:infer`, the binary will be printed as a string if it
      is printable, otherwise in bit syntax.

    * `:charlists` - when `:as_charlists` all lists will be printed as char
      lists, non-printable elements will be escaped.

      When `:as_lists` all lists will be printed as lists.

      When the default `:infer`, the list will be printed as a charlist if it
      is printable, otherwise as list.

    * `:limit` - limits the number of items that are printed for tuples,
      bitstrings, and lists, does not apply to strings nor charlists, defaults
      to 50.

    * `:pretty` - if set to `true` enables pretty printing, defaults to `false`.

    * `:width` - defaults to 80 characters, used when pretty is `true` or when
      printing to IO devices. Set to 0 to force each item to be printed on its
      own line.

    * `:base` - prints integers as `:binary`, `:octal`, `:decimal`, or `:hex`, defaults
      to `:decimal`. When inspecting binaries any `:base` other than `:decimal`
      implies `binaries: :as_binaries`.

    * `:safe` - when `false`, failures while inspecting structs will be raised
      as errors instead of being wrapped in the `Inspect.Error` exception. This
      is useful when debugging failures and crashes for custom inspect
      implementations

    * `:syntax_colors` - when set to a keyword list of colors the output will
      be colorized. The keys are types and the values are the colors to use for
      each type. e.g. `[number: :red, atom: :blue]`. Types can include
      `:number`, `:atom`, `regex`, `:tuple`, `:map`, `:list`, and `:reset`.
      Colors can be any `t:IO.ANSI.ansidata/0` as accepted by `IO.ANSI.format/1`.
  """

  # TODO: Deprecate char_lists key by v1.5
  defstruct structs: true,
            binaries: :infer,
            charlists: :infer,
            char_lists: :infer,
            limit: 50,
            width: 80,
            base: :decimal,
            pretty: false,
            safe: true,
            syntax_colors: []

  @type color_key :: atom

  # TODO: Deprecate char_lists key and :as_char_lists value by v1.5
  @type t :: %__MODULE__{
               structs: boolean,
               binaries: :infer | :as_binaries | :as_strings,
               charlists: :infer | :as_lists | :as_charlists,
               char_lists: :infer | :as_lists | :as_char_lists,
               limit: pos_integer | :infinity,
               width: pos_integer | :infinity,
               base: :decimal | :binary | :hex | :octal,
               pretty: boolean,
               safe: boolean,
               syntax_colors: [{color_key, IO.ANSI.ansidata}]
             }
end

defmodule Inspect.Error do
  @moduledoc """
  Raised when a struct cannot be inspected.
  """
  defexception [:message]
end

defmodule Inspect.Algebra do
  @moduledoc ~S"""
  A set of functions for creating and manipulating algebra
  documents.

  This module implements the functionality described in
  ["Strictly Pretty" (2000) by Christian Lindig][0] with small
  additions, like support for String nodes, and a custom
  rendering function that maximises horizontal space use.

      iex> Inspect.Algebra.empty
      :doc_nil

      iex> "foo"
      "foo"

  With the functions in this module, we can concatenate different
  elements together and render them:

      iex> doc = Inspect.Algebra.concat(Inspect.Algebra.empty, "foo")
      iex> Inspect.Algebra.format(doc, 80)
      ["foo"]

  The functions `nest/2`, `space/2` and `line/2` help you put the
  document together into a rigid structure. However, the document
  algebra gets interesting when using functions like `break/1`, which
  converts the given string into a line break depending on how much space
  there is to print. Let's glue two docs together with a break and then
  render it:

      iex> doc = Inspect.Algebra.glue("a", " ", "b")
      iex> Inspect.Algebra.format(doc, 80)
      ["a", " ", "b"]

  Notice the break was represented as is, because we haven't reached
  a line limit. Once we do, it is replaced by a newline:

      iex> doc = Inspect.Algebra.glue(String.duplicate("a", 20), " ", "b")
      iex> Inspect.Algebra.format(doc, 10)
      ["aaaaaaaaaaaaaaaaaaaa", "\n", "b"]

  Finally, this module also contains Elixir related functions, a bit
  tied to Elixir formatting, namely `surround/3` and `surround_many/5`.

  ## Implementation details

  The original Haskell implementation of the algorithm by [Wadler][1]
  relies on lazy evaluation to unfold document groups on two alternatives:
  `:flat` (breaks as spaces) and `:break` (breaks as newlines).
  Implementing the same logic in a strict language such as Elixir leads
  to an exponential growth of possible documents, unless document groups
  are encoded explicitly as `:flat` or `:break`. Those groups are then reduced
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

  # Functional interface to "doc" records

  @type t :: :doc_nil | :doc_line | doc_cons | doc_nest | doc_break | doc_group | doc_color | binary

  @typep doc_cons :: {:doc_cons, t, t}
  defmacrop doc_cons(left, right) do
    quote do: {:doc_cons, unquote(left), unquote(right)}
  end

  @typep doc_nest :: {:doc_nest, t, non_neg_integer}
  defmacrop doc_nest(doc, indent) do
    quote do: {:doc_nest, unquote(doc), unquote(indent) }
  end

  @typep doc_break :: {:doc_break, binary}
  defmacrop doc_break(break) do
    quote do: {:doc_break, unquote(break)}
  end

  @typep doc_group :: {:doc_group, t}
  defmacrop doc_group(group) do
    quote do: {:doc_group, unquote(group)}
  end

  @typep doc_color :: {:doc_color, t, IO.ANSI.ansidata}
  defmacrop doc_color(doc, color) do
    quote do: {:doc_color, unquote(doc), unquote(color)}
  end

  defmacrop is_doc(doc) do
    if Macro.Env.in_guard?(__CALLER__) do
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
       elem(unquote(doc), 0) in [:doc_cons, :doc_nest, :doc_break, :doc_group, :doc_color])
    end
  end

  @doc """
  Converts an Elixir term to an algebra document
  according to the `Inspect` protocol.
  """
  @spec to_doc(any, Inspect.Opts.t) :: t
  def to_doc(term, opts)

  def to_doc(%{__struct__: struct} = map, %Inspect.Opts{} = opts) when is_atom(struct) do
    if opts.structs do
      try do
        Inspect.inspect(map, opts)
      rescue
        e ->
          stacktrace = System.stacktrace

          # Because we try to raise a nice error message in case
          # we can't inspect a struct, there is a chance the error
          # message itself relies on the struct being printed, so
          # we need to trap the inspected messages to guarantee
          # we won't try to render any failed instruct when building
          # the error message.
          if Process.get(:inspect_trap) do
            Inspect.Map.inspect(map, opts)
          else
            try do
              Process.put(:inspect_trap, true)

              res = Inspect.Map.inspect(map, opts)
              res = IO.iodata_to_binary(format(res, :infinity))

              exception = Inspect.Error.exception(
                message: "got #{inspect e.__struct__} with message " <>
                         "#{inspect Exception.message(e)} while inspecting #{res}"
              )

              if opts.safe do
                Inspect.inspect(exception, opts)
              else
                reraise(exception, stacktrace)
              end
            after
              Process.delete(:inspect_trap)
            end
          end
      end
    else
      Inspect.Map.inspect(map, opts)
    end
  end

  def to_doc(arg, %Inspect.Opts{} = opts) do
    Inspect.inspect(arg, opts)
  end

  @doc """
  Returns a document entity used to represent nothingness.

  ## Examples

      iex> Inspect.Algebra.empty
      :doc_nil

  """
  @spec empty() :: :doc_nil
  def empty, do: :doc_nil

  @doc """
  Concatenates two document entities returning a new document.

  ## Examples

      iex> doc = Inspect.Algebra.concat("hello", "world")
      iex> Inspect.Algebra.format(doc, 80)
      ["hello", "world"]

  """
  @spec concat(t, t) :: t
  def concat(doc1, doc2) when is_doc(doc1) and is_doc(doc2) do
    doc_cons(doc1, doc2)
  end

  @doc """
  Concatenates a list of documents returning a new document.

  ## Examples

      iex> doc = Inspect.Algebra.concat(["a", "b", "c"])
      iex> Inspect.Algebra.format(doc, 80)
      ["a", "b", "c"]

  """
  @spec concat([t]) :: t
  def concat(docs) when is_list(docs) do
    fold_doc(docs, &concat(&1, &2))
  end

  @doc """
  Colors a document if the `color_key` has a color in the options.
  """
  @spec color(t, Inspect.Opts.color_key, Inspect.Opts.t) :: doc_color
  def color(doc, color_key, %Inspect.Opts{syntax_colors: syntax_colors}) when is_doc(doc) do
    if precolor = Keyword.get(syntax_colors, color_key) do
      postcolor = Keyword.get(syntax_colors, :reset, :reset)
      concat(doc_color(doc, precolor), doc_color(empty(), postcolor))
    else
      doc
    end
  end

  @doc ~S"""
  Nests the given document at the given `level`.

  Nesting will be appended to the line breaks.

  ## Examples

      iex> doc = Inspect.Algebra.nest(Inspect.Algebra.glue("hello", "world"), 5)
      iex> Inspect.Algebra.format(doc, 5)
      ["hello", "\n     ", "world"]

  """
  @spec nest(t, non_neg_integer) :: doc_nest
  def nest(doc, level)

  def nest(doc, 0) when is_doc(doc) do
    doc
  end

  def nest(doc, level) when is_doc(doc) and is_integer(level) and level > 0 do
    doc_nest(doc, level)
  end

  @doc ~S"""
  Returns a document entity representing a break based on the given
  `string`.

  This break can be rendered as a linebreak or as the given `string`,
  depending on the `mode` of the chosen layout or the provided
  separator.

  ## Examples

  Let's create a document by concatenating two strings with a break between
  them:

      iex> doc = Inspect.Algebra.concat(["a", Inspect.Algebra.break("\t"), "b"])
      iex> Inspect.Algebra.format(doc, 80)
      ["a", "\t", "b"]

  Notice the break was represented with the given string, because we didn't
  reach a line limit. Once we do, it is replaced by a newline:

      iex> break = Inspect.Algebra.break("\t")
      iex> doc = Inspect.Algebra.concat([String.duplicate("a", 20), break, "b"])
      iex> Inspect.Algebra.format(doc, 10)
      ["aaaaaaaaaaaaaaaaaaaa", "\n", "b"]

  """
  @spec break(binary) :: doc_break
  def break(string) when is_binary(string), do: doc_break(string)

  @doc """
  Returns a document entity representing the default break.

  Same as calling `break/1` with the default break.
  """
  @spec break() :: doc_break
  def break(), do: doc_break(@break)

  @doc """
  Glues two documents together inserting the default break between them.

  The break that is inserted between `left` and `right` is the one returned by
  `break/0`.

  ## Examples

      iex> doc = Inspect.Algebra.glue("hello", "world")
      iex> Inspect.Algebra.format(doc, 80)
      ["hello", " ", "world"]

  """
  @spec glue(t, t) :: t
  def glue(doc1, doc2), do: concat(doc1, concat(break(), doc2))

  @doc """
  Glues two documents (`doc1` and `doc2`) together inserting the given
  break `break_string` between them.

  For more information on how the break is inserted, see `break/1`.

  ## Examples

      iex> doc = Inspect.Algebra.glue("hello", "\t", "world")
      iex> Inspect.Algebra.format(doc, 80)
      ["hello", "\t", "world"]

  """
  @spec glue(t, binary, t) :: t
  def glue(doc1, break_string, doc2) when is_binary(break_string),
    do: concat(doc1, concat(break(break_string), doc2))

  @doc ~S"""
  Returns a group containing the specified document `doc`.

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
      iex> Inspect.Algebra.format(doc, 80)
      ["Hello,", " ", "A", " ", "B"]
      iex> Inspect.Algebra.format(doc, 6)
      ["Hello,", "\n", "A", " ", "B"]

  """
  @spec group(t) :: doc_group
  def group(doc) when is_doc(doc) do
    doc_group(doc)
  end

  @doc """
  Inserts a mandatory single space between two documents.

  ## Examples

      iex> doc = Inspect.Algebra.space("Hughes", "Wadler")
      iex> Inspect.Algebra.format(doc, 5)
      ["Hughes", " ", "Wadler"]

  """
  @spec space(t, t) :: t
  def space(doc1, doc2), do: concat(doc1, concat(" ", doc2))

  @doc ~S"""
  Inserts a mandatory linebreak between two documents.

  ## Examples

      iex> doc = Inspect.Algebra.line("Hughes", "Wadler")
      iex> Inspect.Algebra.format(doc, 80)
      ["Hughes", "\n", "Wadler"]

  """
  @spec line(t, t) :: t
  def line(doc1, doc2), do: concat(doc1, concat(:doc_line, doc2))

  @doc """
  Folds a list of documents into a document using the given folder function.

  The list of documents is folded "from the right"; in that, this function is
  similar to `List.foldr/3`, except that it doesn't expect an initial
  accumulator and uses the last element of `docs` as the initial accumulator.

  ## Examples

      iex> docs = ["A", "B", "C"]
      iex> docs = Inspect.Algebra.fold_doc(docs, fn(doc, acc) ->
      ...>   Inspect.Algebra.concat([doc, "!", acc])
      ...> end)
      iex> Inspect.Algebra.format(docs, 80)
      ["A", "!", "B", "!", "C"]

  """
  @spec fold_doc([t], ((t, t) -> t)) :: t
  def fold_doc(docs, folder_fun)

  def fold_doc([], _folder_fun),
    do: empty()
  def fold_doc([doc], _folder_fun),
    do: doc
  def fold_doc([doc | docs], folder_fun) when is_function(folder_fun, 2),
    do: folder_fun.(doc, fold_doc(docs, folder_fun))

  # Elixir conveniences

  @doc ~S"""
  Surrounds a document with characters.

  Puts the given document `doc` between the `left` and `right` documents enclosing
  and nesting it. The document is marked as a group, to show the maximum as
  possible concisely together.

  ## Examples

      iex> doc = Inspect.Algebra.surround("[", Inspect.Algebra.glue("a", "b"), "]")
      iex> Inspect.Algebra.format(doc, 3)
      ["[", "a", "\n ", "b", "]"]

  """
  @spec surround(t, t, t) :: t
  def surround(left, doc, right) when is_doc(left) and is_doc(doc) and is_doc(right) do
    group(concat(left, concat(nest(doc, @nesting), right)))
  end

  @doc ~S"""
  Maps and glues a collection of items.

  It uses the given `left` and `right` documents as surrounding and the
  separator document `separator` to separate items in `docs`. A limit can be
  passed: when this limit is reached, this function stops gluing and outputs
  `"..."` instead.

  ## Examples

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]",
      ...>         %Inspect.Opts{limit: :infinity}, fn i, _opts -> to_string(i) end)
      iex> Inspect.Algebra.format(doc, 5) |> IO.iodata_to_binary
      "[1,\n 2,\n 3,\n 4,\n 5]"

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]",
      ...>         %Inspect.Opts{limit: 3}, fn i, _opts -> to_string(i) end)
      iex> Inspect.Algebra.format(doc, 20) |> IO.iodata_to_binary
      "[1, 2, 3, ...]"

      iex> doc = Inspect.Algebra.surround_many("[", Enum.to_list(1..5), "]",
      ...>         %Inspect.Opts{limit: 3}, fn i, _opts -> to_string(i) end, "!")
      iex> Inspect.Algebra.format(doc, 20) |> IO.iodata_to_binary
      "[1! 2! 3! ...]"

  """
  @spec surround_many(t, [any], t, Inspect.Opts.t, (term, Inspect.Opts.t -> t), t) :: t
  def surround_many(left, docs, right, %Inspect.Opts{} = opts, fun, separator \\ @surround_separator)
      when is_doc(left) and is_list(docs) and is_doc(right) and is_function(fun, 2) and is_doc(separator) do
    do_surround_many(left, docs, right, opts.limit, opts, fun, separator)
  end

  defp do_surround_many(left, [], right, _, _opts, _fun, _) do
    concat(left, right)
  end

  defp do_surround_many(left, docs, right, limit, opts, fun, sep) do
    surround(left, do_surround_many(docs, limit, opts, fun, sep), right)
  end

  defp do_surround_many(_, 0, _opts, _fun, _sep) do
    "..."
  end

  defp do_surround_many([], _limit, _opts, _fun, _sep) do
    :doc_nil
  end

  defp do_surround_many([h], limit, opts, fun, _sep) do
    fun.(h, %{opts | limit: limit})
  end

  defp do_surround_many([h | t], limit, opts, fun, sep) when is_list(t) do
    limit = decrement(limit)
    h = fun.(h, %{opts | limit: limit})
    t = do_surround_many(t, limit, opts, fun, sep)
    do_join(h, t, sep)
  end

  defp do_surround_many([h | t], limit, opts, fun, _sep) do
    limit = decrement(limit)
    h = fun.(h, %{opts | limit: limit})
    t = fun.(t, %{opts | limit: limit})
    do_join(h, t, @tail_separator)
  end

  defp do_join(:doc_nil, :doc_nil, _), do: :doc_nil
  defp do_join(h, :doc_nil, _),        do: h
  defp do_join(:doc_nil, t, _),        do: t
  defp do_join(h, t, sep),             do: glue(concat(h, sep), t)

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1

  @doc ~S"""
  Formats a given document for a given width.

  Takes the maximum width and a document to print as its arguments
  and returns an IO data representation of the best layout for the
  document to fit in the given width.

  ## Examples

      iex> doc = Inspect.Algebra.glue("hello", " ", "world")
      iex> Inspect.Algebra.format(doc, 30) |> IO.iodata_to_binary()
      "hello world"
      iex> Inspect.Algebra.format(doc, 10) |> IO.iodata_to_binary()
      "hello\nworld"

  """
  @spec format(t, non_neg_integer | :infinity) :: iodata
  def format(doc, width) when is_doc(doc) and (width == :infinity or width >= 0) do
    format(width, 0, [{0, default_mode(width), doc_group(doc)}])
  end

  defp default_mode(:infinity), do: :flat
  defp default_mode(_),         do: :break

  # Record representing the document mode to be rendered: flat or broken
  @typep mode :: :flat | :break

  @spec fits?(integer, [{integer, mode, t}]) :: boolean
  defp fits?(w, _) when w < 0,                      do: false
  defp fits?(_, []),                                do: true
  defp fits?(_, [{_, _, :doc_line} | _]),           do: true
  defp fits?(w, [{_, _, :doc_nil} | t]),            do: fits?(w, t)
  defp fits?(w, [{i, m, doc_cons(x, y)} | t]),      do: fits?(w, [{i, m, x} | [{i, m, y} | t]])
  defp fits?(w, [{i, m, doc_color(x, _)} | t]),    do: fits?(w, [{i, m, x} | t])
  defp fits?(w, [{i, m, doc_nest(x, j)} | t]),      do: fits?(w, [{i + j, m, x} | t])
  defp fits?(w, [{i, _, doc_group(x)} | t]),        do: fits?(w, [{i, :flat, x} | t])
  defp fits?(w, [{_, _, s} | t]) when is_binary(s), do: fits?((w - byte_size(s)), t)
  defp fits?(w, [{_, :flat, doc_break(s)} | t]),    do: fits?((w - byte_size(s)), t)
  defp fits?(_, [{_, :break, doc_break(_)} | _]),   do: true

  @spec format(integer | :infinity, integer, [{integer, mode, t}]) :: [binary]
  defp format(_, _, []),                                do: []
  defp format(w, _, [{i, _, :doc_line} | t]),           do: [indent(i) | format(w, i, t)]
  defp format(w, k, [{_, _, :doc_nil} | t]),            do: format(w, k, t)
  defp format(w, k, [{i, m, doc_cons(x, y)} | t]),      do: format(w, k, [{i, m, x} | [{i, m, y} | t]])
  defp format(w, k, [{i, m, doc_nest(x, j)} | t]),      do: format(w, k, [{i + j, m, x} | t])
  defp format(w, k, [{i, m, doc_group(x)} | t]),        do: format(w, k, [{i, m, x} | t])
  defp format(w, k, [{i, m, doc_color(x, c)} | t]),     do: [ansi(c) | format(w, k, [{i, m, x} | t])]
  defp format(w, k, [{_, _, s} | t]) when is_binary(s), do: [s | format(w, (k + byte_size(s)), t)]
  defp format(w, k, [{_, :flat, doc_break(s)} | t]),    do: [s | format(w, (k + byte_size(s)), t)]
  defp format(w, k, [{i, :break, doc_break(s)} | t]) do
    k = k + byte_size(s)

    if w == :infinity or fits?(w - k, t) do
      [s | format(w, k, t)]
    else
      [indent(i) | format(w, i, t)]
    end
  end

  defp ansi(color) do
    IO.ANSI.format_fragment(color, true)
  end

  defp indent(0), do: @newline
  defp indent(i), do: @newline <> :binary.copy(" ", i)
end
