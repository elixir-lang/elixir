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
      bitstrings, maps, lists and any other collection of items. It does not
      apply to strings nor charlists and defaults to 50.

    * `:printable_limit` - limits the number of bytes that are printed for strings
      and char lists. Defaults to 4096.

    * `:pretty` - if set to `true` enables pretty printing, defaults to `false`.

    * `:width` - defaults to 80 characters, used when pretty is `true` or when
      printing to IO devices. Set to 0 to force each item to be printed on its
      own line.

    * `:base` - prints integers as `:binary`, `:octal`, `:decimal`, or `:hex`,
      defaults to `:decimal`. When inspecting binaries any `:base` other than
      `:decimal` implies `binaries: :as_binaries`.

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

  # TODO: Remove :char_lists key by 2.0
  defstruct structs: true,
            binaries: :infer,
            charlists: :infer,
            char_lists: :infer,
            limit: 50,
            printable_limit: 4096,
            width: 80,
            base: :decimal,
            pretty: false,
            safe: true,
            syntax_colors: []

  @type color_key :: atom

  # TODO: Remove :char_lists key and :as_char_lists value by 2.0
  @type t :: %__MODULE__{
               structs: boolean,
               binaries: :infer | :as_binaries | :as_strings,
               charlists: :infer | :as_lists | :as_charlists,
               char_lists: :infer | :as_lists | :as_char_lists,
               limit: pos_integer | :infinity,
               printable_limit: pos_integer | :infinity,
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
  additions, like support for binary nodes and a break mode that
  maximises use of horizontal space.

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
  algebra gets interesting when using functions like `glue/3` and
  `group/1`. A glue inserts a break between two documents. A group
  indicates a document that must fit the current line, otherwise
  breaks are rendered as new lines. Let's glue two docs together
  with a break, group it and then render it:

      iex> doc = Inspect.Algebra.glue("a", " ", "b")
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 80)
      ["a", " ", "b"]

  Notice the break was represented as is, because we haven't reached
  a line limit. Once we do, it is replaced by a newline:

      iex> doc = Inspect.Algebra.glue(String.duplicate("a", 20), " ", "b")
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 10)
      ["aaaaaaaaaaaaaaaaaaaa", "\n", "b"]

  This module uses the byte size to compute how much space there is
  left. If your document contains strings, then those need to be
  wrapped in `string/1`, which then relies on `String.length/1` to
  precompute the document size.

  Finally, this module also contains Elixir related functions, a bit
  tied to Elixir formatting, namely `surround/3` and `surround_many/5`.

  ## Implementation details

  The original Haskell implementation of the algorithm by [Wadler][1]
  relies on lazy evaluation to unfold document groups on two alternatives:
  `:flat` (breaks as spaces) and `:break` (breaks as newlines).
  Implementing the same logic in a strict language such as Elixir leads
  to an exponential growth of possible documents, unless groups are explicitly
  encoded. Those groups are then reduced to a simple document, where the
  layout is already decided, per [Lindig][0].

  This implementation has two types of breaks: `:strict` and `:flex`. When
  a group does not fit, all strict breaks are treated as breaks. The flex
  breaks however are re-evaluated and may still be rendered as spaces.

  This implementation also adds `force_break/1` and `cancel_break/2` which
  give more control over the document fitting.

  Custom pretty printers can be implemented using the documents returned
  by this module and by providing their own rendering functions.

    [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
    [1]: http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

  """

  @surround_separator ","
  @tail_separator " |"
  @newline "\n"
  @break :flex
  @cancel_break :enabled

  # Functional interface to "doc" records

  @type t ::
          binary
          | :doc_nil
          | :doc_line
          | doc_string
          | doc_cons
          | doc_nest
          | doc_break
          | doc_group
          | doc_color
          | doc_force
          | doc_cancel
          | doc_collapse

  @typep doc_string :: {:doc_string, t, non_neg_integer}
  defmacrop doc_string(string, length) do
    quote do: {:doc_string, unquote(string), unquote(length)}
  end

  @typep doc_cons :: {:doc_cons, t, t}
  defmacrop doc_cons(left, right) do
    quote do: {:doc_cons, unquote(left), unquote(right)}
  end

  @typep doc_nest :: {:doc_nest, t, :cursor | :reset | non_neg_integer, :always | :break}
  defmacrop doc_nest(doc, indent, always_or_break) do
    quote do: {:doc_nest, unquote(doc), unquote(indent), unquote(always_or_break)}
  end

  @typep doc_break :: {:doc_break, binary, :flex | :strict}
  defmacrop doc_break(break, mode) do
    quote do: {:doc_break, unquote(break), unquote(mode)}
  end

  @typep doc_group :: {:doc_group, t}
  defmacrop doc_group(group) do
    quote do: {:doc_group, unquote(group)}
  end

  @typep doc_cancel :: {:doc_cancel, t, :enabled | :disabled}
  defmacrop doc_cancel(group, mode) do
    quote do: {:doc_cancel, unquote(group), unquote(mode)}
  end

  @typep doc_force :: {:doc_force, t}
  defmacrop doc_force(group) do
    quote do: {:doc_force, unquote(group)}
  end

  @typep doc_collapse :: {:doc_collapse, pos_integer()}
  defmacrop doc_collapse(count) do
    quote do: {:doc_collapse, unquote(count)}
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
       elem(unquote(doc), 0) in [:doc_string, :doc_cons, :doc_nest, :doc_break, :doc_group,
                                 :doc_color, :doc_force, :doc_cancel, :doc_collapse])
    end
  end

  @doc """
  Converts an Elixir term to an algebra document
  according to the `Inspect` protocol.
  """
  @spec to_doc(any, Inspect.Opts.t) :: t
  def to_doc(term, opts)

  def to_doc(%_{} = struct, %Inspect.Opts{} = opts) do
    if opts.structs do
      try do
        Inspect.inspect(struct, opts)
      rescue
        caught_exception ->
          stacktrace = System.stacktrace

          # Because we try to raise a nice error message in case
          # we can't inspect a struct, there is a chance the error
          # message itself relies on the struct being printed, so
          # we need to trap the inspected messages to guarantee
          # we won't try to render any failed instruct when building
          # the error message.
          if Process.get(:inspect_trap) do
            Inspect.Map.inspect(struct, opts)
          else
            try do
              Process.put(:inspect_trap, true)

              res = Inspect.Map.inspect(struct, %{opts | syntax_colors: []})
              res = IO.iodata_to_binary(format(res, :infinity))

              message =
                "got #{inspect caught_exception.__struct__} with message " <>
                "#{inspect Exception.message(caught_exception)} while inspecting #{res}"
              exception = Inspect.Error.exception(message: message)

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
      Inspect.Map.inspect(struct, opts)
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

  @doc ~S"""
  Creates a document represented by string.

  While `Inspect.Algebra` accepts binaries as documents,
  those are counted by binary size. On the other hand,
  `string` documents are measured in terms of graphemes
  towards the document size.

  ## Examples

  The following document has 10 bytes and therefore it
  does not format to width 9 without breaks:

      iex> doc = Inspect.Algebra.glue("olá", " ", "mundo")
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 9)
      ["olá", "\n", "mundo"]

  However, if we use `string`, then the string length is
  used, instead of byte size, correctly fitting:

      iex> string = Inspect.Algebra.string("olá")
      iex> doc = Inspect.Algebra.glue(string, " ", "mundo")
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 9)
      ["olá", " ", "mundo"]

  """
  @spec string(String.t) :: doc_string
  def string(string) when is_binary(string) do
    doc_string(string, String.length(string))
  end

  @doc ~S"""
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

  @doc ~S"""
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

  @doc ~S"""
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

  If `level` is an integer, that's the indentation appended
  to line breaks whenever they occur. If the level is `:cursor`,
  the current position of the "cursor" in the document becomes
  the nesting. If the level is `:reset`, it is set back to 0.

  `mode` can be `:always`, which means nesting always happen,
  or `:break`, which means nesting only happens inside a group
  that has been broken.

  ## Examples

      iex> doc = Inspect.Algebra.nest(Inspect.Algebra.glue("hello", "world"), 5)
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 5)
      ["hello", "\n     ", "world"]

  """
  @spec nest(t, non_neg_integer) :: doc_nest
  def nest(doc, level, mode \\ :always)

  def nest(doc, :cursor, mode) when is_doc(doc) and mode in [:always, :break] do
    doc_nest(doc, :cursor, mode)
  end

  def nest(doc, :reset, mode) when is_doc(doc) and mode in [:always, :break] do
    doc_nest(doc, :reset, mode)
  end

  def nest(doc, 0, _mode) when is_doc(doc) do
    doc
  end

  def nest(doc, level, mode)
      when is_doc(doc) and is_integer(level) and level > 0 and mode in [:always, :break] do
    doc_nest(doc, level, mode)
  end

  @doc ~S"""
  Returns a document entity representing a break based on the given
  `string`.

  This break can be rendered as a linebreak or as the given `string`,
  depending on the `mode` of the chosen layout.

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
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 10)
      ["aaaaaaaaaaaaaaaaaaaa", "\n", "b"]

  """
  @spec break(binary) :: doc_break
  def break(string \\ " ") when is_binary(string) do
    doc_break(string, :strict)
  end

  @doc """
  Collapse any new lines and whitespace following this
  node and emitting up to `max` new lines.
  """
  @spec collapse_lines(pos_integer) :: doc_collapse
  def collapse_lines(max) when is_integer(max) and max > 0 do
    doc_collapse(max)
  end

  @doc """
  Considers the next break as fit.

  `mode` can be `:enabled` or `:disabled`. When `:enabled`,
  it will consider the document as fit as soon as it finds
  the next break, effectively cancelling the break. It will
  also ignore any `force_break/1`.

  When disabled, it behaves as usual and it will ignore
  any further `cancel_break/2` instruction.
  """
  @spec cancel_break(t) :: doc_cancel
  def cancel_break(doc, mode \\ @cancel_break) when is_doc(doc) do
    doc_cancel(doc, mode)
  end

  @doc """
  Forces the document to break.
  """
  @spec force_break(t) :: doc_force
  def force_break(doc) when is_doc(doc) do
    doc_force(doc)
  end

  @doc """
  Introduces a flex break.

  A flex break still causes a group to break, like
  a regular break, but it is re-evaluated when the
  documented is processed.

  This function is used by `surround/4` and friends
  to the maximum number of entries on the same line.
  """
  @spec flex_break(binary) :: doc_break
  def flex_break(string \\ " ") when is_binary(string) do
    doc_break(string, :flex)
  end

  @doc """
  Glues two documents (`doc1` and `doc2`) inserting a
  `flex_break/1` given by `break_string` between them.

  This function is used by `surround/4` and friends
  to the maximum number of entries on the same line.
  """
  @spec flex_glue(t, binary, t) :: t
  def flex_glue(doc1, break_string \\ " ", doc2) when is_binary(break_string) do
    concat(doc1, concat(flex_break(break_string), doc2))
  end

  @doc ~S"""
  Glues two documents (`doc1` and `doc2`) inserting the given
  break `break_string` between them.

  For more information on how the break is inserted, see `break/1`.

  ## Examples

      iex> doc = Inspect.Algebra.glue("hello", "world")
      iex> Inspect.Algebra.format(doc, 80)
      ["hello", " ", "world"]

      iex> doc = Inspect.Algebra.glue("hello", "\t", "world")
      iex> Inspect.Algebra.format(doc, 80)
      ["hello", "\t", "world"]

  """
  @spec glue(t, binary, t) :: t
  def glue(doc1, break_string \\ " ", doc2) when is_binary(break_string) do
    concat(doc1, concat(break(break_string), doc2))
  end

  @doc ~S"""
  Returns a group containing the specified document `doc`.

  Documents in a group are attempted to be rendered together
  to the best of the renderer ability.

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
      ["Hello,", "\n", "A", "\n", "B"]

  """
  @spec group(t) :: doc_group
  def group(doc) when is_doc(doc) do
    doc_group(doc)
  end

  @doc ~S"""
  Inserts a mandatory single space between two documents.

  ## Examples

      iex> doc = Inspect.Algebra.space("Hughes", "Wadler")
      iex> Inspect.Algebra.format(doc, 5)
      ["Hughes", " ", "Wadler"]

  """
  @spec space(t, t) :: t
  def space(doc1, doc2), do: concat(doc1, concat(" ", doc2))

  @doc ~S"""
  A mandatory linebreak.

  A group with linebreaks will fit if all lines in the group fit.

  ## Examples

    iex> doc = Inspect.Algebra.concat(
    ...>   Inspect.Algebra.concat(
    ...>     "Hughes",
    ...>     Inspect.Algebra.line()
    ...>   ), "Wadler"
    ...> )
    iex> Inspect.Algebra.format(doc, 80)
    ["Hughes", "\n", "Wadler"]

  """
  @spec line() :: t
  def line(), do: :doc_line

  @doc ~S"""
  Inserts a mandatory linebreak between two documents.

  See `line/1`.

  ## Examples

      iex> doc = Inspect.Algebra.line("Hughes", "Wadler")
      iex> Inspect.Algebra.format(doc, 80)
      ["Hughes", "\n", "Wadler"]

  """
  @spec line(t, t) :: t
  def line(doc1, doc2), do: concat(doc1, concat(line(), doc2))

  @doc ~S"""
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

  ## Options

    * `:break` - controls if the break is `:strict` or `:flex`, see `group/2`

  ## Examples

      iex> doc = Inspect.Algebra.surround("[", Inspect.Algebra.glue("a", "b"), "]")
      iex> doc = Inspect.Algebra.group(doc)
      iex> Inspect.Algebra.format(doc, 3)
      ["[", "a", "\n ", "b", "]"]

  """
  # TODO: Reflect the @break default and nesting for flex on the formatter
  @spec surround(t, t, t, keyword()) :: t
  def surround(left, doc, right, opts \\ [])
      when is_doc(left) and is_doc(doc) and is_doc(right) and is_list(opts) do
    case Keyword.get(opts, :break, @break) do
      :flex ->
        concat(concat(left, nest(doc, 1)), right)
      :strict ->
        group(glue(nest(glue(left, "", doc), 2), "", right))
    end
  end

  @doc ~S"""
  Maps and glues a collection of items.

  It uses the given `left` and `right` documents as surrounding and the
  separator document `separator` to separate items in `docs`. A limit can be
  passed: when this limit is reached, this function stops gluing and outputs
  `"..."` instead.

  ## Options

    * `:separator` - the separator used between each doc

  Plus all options in `surround/4`.

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
  @spec surround_many(t, [any], t, Inspect.Opts.t, (term, Inspect.Opts.t -> t), keyword) :: t
  def surround_many(left, docs, right, %Inspect.Opts{} = inspect, fun, opts \\ [])
      when is_doc(left) and is_list(docs) and is_doc(right) and is_function(fun, 2) do
    cond do
      is_list(opts) ->
        {separator, opts} = Keyword.pop(opts, :separator, @surround_separator)
        surround_many(left, docs, right, inspect, fun, separator, opts)
      is_doc(opts) ->
        # TODO: Deprecate on Elixir v1.8
        surround_many(left, docs, right, inspect, fun, opts, [])
    end
  end

  defp surround_many(left, [], right, _inspect, _fun, _sep, _opts) do
    concat(left, right)
  end

  defp surround_many(left, docs, right, inspect, fun, sep, opts) do
    break = Keyword.get(opts, :break, @break)
    surround(left, surround_each(docs, inspect.limit, inspect, fun, break, sep), right, opts)
  end

  defp surround_each(_, 0, _opts, _fun, _break, _sep) do
    "..."
  end

  defp surround_each([], _limit, _opts, _fun, _break, _sep) do
    :doc_nil
  end

  defp surround_each([h], limit, opts, fun, _break, _sep) do
    fun.(h, %{opts | limit: limit})
  end

  defp surround_each([h | t], limit, opts, fun, break, sep) when is_list(t) do
    limit = decrement(limit)
    h = fun.(h, %{opts | limit: limit})
    t = surround_each(t, limit, opts, fun, break, sep)
    do_join(h, t, break, sep)
  end

  defp surround_each([h | t], limit, opts, fun, break, _sep) do
    limit = decrement(limit)
    h = fun.(h, %{opts | limit: limit})
    t = fun.(t, %{opts | limit: limit})
    do_join(h, t, break, @tail_separator)
  end

  defp do_join(:doc_nil, :doc_nil, _, _), do: :doc_nil
  defp do_join(h, :doc_nil, _, _), do: h
  defp do_join(:doc_nil, t, _, _), do: t
  defp do_join(h, t, :strict, sep), do: glue(concat(h, sep), t)
  defp do_join(h, t, :flex, sep), do: flex_glue(concat(h, sep), t)

  defp decrement(:infinity), do: :infinity
  defp decrement(counter),   do: counter - 1

  @doc ~S"""
  Formats a given document for a given width.

  Takes the maximum width and a document to print as its arguments
  and returns an IO data representation of the best layout for the
  document to fit in the given width.

  The document starts flat (without breaks) until a group is found.

  ## Examples

      iex> doc = Inspect.Algebra.glue("hello", " ", "world")
      iex> doc = Inspect.Algebra.group(doc)
      iex> doc |> Inspect.Algebra.format(30) |> IO.iodata_to_binary()
      "hello world"
      iex> doc |> Inspect.Algebra.format(10) |> IO.iodata_to_binary()
      "hello\nworld"

  """
  @spec format(t, non_neg_integer | :infinity) :: iodata
  def format(doc, width) when is_doc(doc) and (width == :infinity or width >= 0) do
    format(width, 0, [{0, :flat, doc}])
  end

  # Type representing the document mode to be rendered
  #
  #   * break - represents a fitted document with breaks as breaks
  #   * flat - represents a fitted document with breaks as flats
  #   * cancel - represents a document being fitted that will cancel (fit) the next break
  #   * no_cancel - represents a document being fitted that will not accept cancelations
  #
  @typep mode :: :break | :flat | :cancel | :no_cancel

  @spec fits?(integer, integer, [{integer, mode, t}]) :: boolean
  defp fits?(w, k, _) when k > w,                            do: false
  defp fits?(_, _, []),                                      do: true

  defp fits?(w, k, [{i, _, doc_cancel(x, :disabled)} | t]),  do: fits?(w, k, [{i, :no_cancel, x} | t])
  defp fits?(w, k, [{i, :no_cancel, doc_group(x)} | t]),     do: fits?(w, k, [{i, :no_cancel, x} | t])
  defp fits?(w, k, [{i, :no_cancel, doc_cancel(x, _)} | t]), do: fits?(w, k, [{i, :no_cancel, x} | t])

  defp fits?(w, k, [{i, _, doc_cancel(x, :enabled)} | t]),   do: fits?(w, k, [{i, :cancel, x} | t])
  defp fits?(w, k, [{i, :cancel, doc_force(x)} | t]),        do: fits?(w, k, [{i, :cancel, x} | t])
  defp fits?(w, k, [{i, :cancel, doc_group(x)} | t]),        do: fits?(w, k, [{i, :cancel, x} | t])
  defp fits?(_, _, [{_, :cancel, doc_break(_, _)} | _]),     do: true
  defp fits?(_, _, [{_, :cancel, :doc_line} | _]),           do: true

  defp fits?(w, k, [{_, _, :doc_nil} | t]),                  do: fits?(w, k, t)
  defp fits?(w, _, [{i, _, :doc_line} | t]),                 do: fits?(w, i, t)
  defp fits?(w, _, [{i, _, doc_collapse(_)} | t]),           do: fits?(w, i, t)
  defp fits?(w, k, [{i, m, doc_cons(x, y)} | t]),            do: fits?(w, k, [{i, m, x} | [{i, m, y} | t]])
  defp fits?(w, k, [{i, m, doc_color(x, _)} | t]),           do: fits?(w, k, [{i, m, x} | t])
  defp fits?(w, k, [{i, m, doc_nest(x, _, :break)} | t]),    do: fits?(w, k, [{i, m, x} | t])
  defp fits?(w, k, [{i, m, doc_nest(x, j, _)} | t]),         do: fits?(w, k, [{apply_nesting(i, k, j), m, x} | t])
  defp fits?(w, k, [{i, _, doc_group(x)} | t]),              do: fits?(w, k, [{i, :flat, x} | t])
  defp fits?(w, k, [{_, _, doc_string(_, l)} | t]),          do: fits?(w, k + l, t)
  defp fits?(w, k, [{_, _, s} | t]) when is_binary(s),       do: fits?(w, k + byte_size(s), t)
  defp fits?(_, _, [{_, _, doc_force(_)} | _]),              do: false
  defp fits?(_, _, [{_, :break, doc_break(_, _)} | _]),      do: true
  defp fits?(w, k, [{_, _, doc_break(s, _)} | t]),           do: fits?(w, k + byte_size(s), t)

  @spec format(integer | :infinity, integer, [{integer, mode, t}]) :: [binary]
  defp format(_, _, []),                                do: []
  defp format(w, k, [{_, _, :doc_nil} | t]),            do: format(w, k, t)
  defp format(w, _, [{i, _, :doc_line} | t]),           do: [indent(i) | format(w, i, t)]
  defp format(w, k, [{i, m, doc_cons(x, y)} | t]),      do: format(w, k, [{i, m, x} | [{i, m, y} | t]])
  defp format(w, k, [{i, m, doc_color(x, c)} | t]),     do: [ansi(c) | format(w, k, [{i, m, x} | t])]
  defp format(w, k, [{_, _, doc_string(s, l)} | t]),    do: [s | format(w, k + l, t)]
  defp format(w, k, [{_, _, s} | t]) when is_binary(s), do: [s | format(w, k + byte_size(s), t)]
  defp format(w, k, [{i, m, doc_force(x)} | t]),        do: format(w, k, [{i, m, x} | t])
  defp format(w, k, [{i, m, doc_cancel(x, _)} | t]),    do: format(w, k, [{i, m, x} | t])
  defp format(w, _, [{i, _, doc_collapse(max)} | t]),   do: collapse(format(w, i, t), max, 0, i)

  # Flex breaks are not conditional to the mode
  defp format(w, k, [{i, _, doc_break(s, :flex)} | t]) do
    k = k + byte_size(s)

    if w == :infinity or fits?(w, k, t) do
      [s | format(w, k, t)]
    else
      [indent(i) | format(w, i, t)]
    end
  end

  # Strict breaks are conditional to the mode
  defp format(w, k, [{i, mode, doc_break(s, :strict)} | t]) do
    if mode == :break do
      [indent(i) | format(w, i, t)]
    else
      [s | format(w, k + byte_size(s), t)]
    end
  end

  # Nesting is conditional to the mode.
  defp format(w, k, [{i, mode, doc_nest(x, j, nest)} | t]) do
    if nest == :always or (nest == :break and mode == :break) do
      format(w, k, [{apply_nesting(i, k, j), mode, x} | t])
    else
      format(w, k, [{i, mode, x} | t])
    end
  end

  # Groups must do the fitting decision.
  defp format(w, k, [{i, _, doc_group(x)} | t]) do
    if w == :infinity or fits?(w, k, [{i, :flat, x}]) do
      format(w, k, [{i, :flat, x} | t])
    else
      format(w, k, [{i, :break, x} | t])
    end
  end

  defp collapse(["\n" <> _ | t], max, count, i) do
    collapse(t, max, count + 1, i)
  end
  defp collapse(["" | t], max, count, i) do
    collapse(t, max, count, i)
  end
  defp collapse(t, max, count, i) do
    [:binary.copy("\n", min(max, count)) <> :binary.copy(" ", i) | t]
  end

  defp apply_nesting(_, k, :cursor), do: k
  defp apply_nesting(_, _, :reset), do: 0
  defp apply_nesting(i, _, j), do: i + j

  defp ansi(color) do
    IO.ANSI.format_fragment(color, true)
  end

  defp indent(0), do: @newline
  defp indent(i), do: @newline <> :binary.copy(" ", i)
end
