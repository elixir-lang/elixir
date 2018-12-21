defmodule Inspect.Opts do
  @moduledoc """
  Defines the options used by the `Inspect` protocol.

  The following fields are available:

    * `:structs` - when `false`, structs are not formatted by the inspect
      protocol, they are instead printed as maps, defaults to `true`.

    * `:binaries` - when `:as_strings` all binaries will be printed as strings,
      non-printable bytes will be escaped.

      When `:as_binaries` all binaries will be printed in bit syntax.

      When the default `:infer`, the binary will be printed as a string if it
      is printable, otherwise in bit syntax.

    * `:charlists` - when `:as_charlists` all lists will be printed as charlists,
      non-printable elements will be escaped.

      When `:as_lists` all lists will be printed as lists.

      When the default `:infer`, the list will be printed as a charlist if it
      is printable, otherwise as list.

    * `:limit` - limits the number of items that are printed for tuples,
      bitstrings, maps, lists and any other collection of items. It does not
      apply to strings nor charlists and defaults to 50. If you don't want to limit
      the number of items to a particular number, use `:infinity`.

    * `:printable_limit` - limits the number of bytes that are printed for strings
      and charlists. Defaults to 4096. If you don't want to limit the number of items
      to a particular number, use `:infinity`.

    * `:pretty` - if set to `true` enables pretty printing, defaults to `false`.

    * `:width` - defaults to 80 characters, used when pretty is `true` or when
      printing to IO devices. Set to 0 to force each item to be printed on its
      own line. If you don't want to limit the number of items to a particular
      number, use `:infinity`.

    * `:base` - prints integers as `:binary`, `:octal`, `:decimal`, or `:hex`,
      defaults to `:decimal`. When inspecting binaries any `:base` other than
      `:decimal` implies `binaries: :as_binaries`.

    * `:safe` - when `false`, failures while inspecting structs will be raised
      as errors instead of being wrapped in the `Inspect.Error` exception. This
      is useful when debugging failures and crashes for custom inspect
      implementations

    * `:syntax_colors` - when set to a keyword list of colors the output will
      be colorized. The keys are types and the values are the colors to use for
      each type (for example, `[number: :red, atom: :blue]`). Types can include
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
          syntax_colors: [{color_key, IO.ANSI.ansidata()}]
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

      iex> Inspect.Algebra.empty()
      :doc_nil

      iex> "foo"
      "foo"

  With the functions in this module, we can concatenate different
  elements together and render them:

      iex> doc = Inspect.Algebra.concat(Inspect.Algebra.empty(), "foo")
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
  tied to Elixir formatting, such as `to_doc/2`.

  ## Implementation details

  The implementation of `Inspect.Algebra` is based on the Strictly Pretty
  paper by [Lindig][0] which builds on top of previous pretty printing
  algorithms but is tailored to strict languages, such as Elixir.
  The core idea in the paper is the use of explicit document groups which
  are rendered as flat (breaks as spaces) or as break (breaks as newlines).

  This implementation provides two types of breaks: `:strict` and `:flex`.
  When a group does not fit, all strict breaks are treated as newlines.
  Flex breaks however are re-evaluated on every occurrence and may still
  be rendered flat. See `break/1` and `flex_break/1` for more information.

  This implementation also adds `force_unfit/1` and `next_break_fits/2` which
  give more control over the document fitting.

    [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200

  """

  @container_separator ","
  @tail_separator " |"
  @newline "\n"
  @next_break_fits :enabled

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
          | doc_fits
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

  @typep doc_group :: {:doc_group, t, :inherit | :self}
  defmacrop doc_group(group, mode) do
    quote do: {:doc_group, unquote(group), unquote(mode)}
  end

  @typep doc_fits :: {:doc_fits, t, :enabled | :disabled}
  defmacrop doc_fits(group, mode) do
    quote do: {:doc_fits, unquote(group), unquote(mode)}
  end

  @typep doc_force :: {:doc_force, t}
  defmacrop doc_force(group) do
    quote do: {:doc_force, unquote(group)}
  end

  @typep doc_collapse :: {:doc_collapse, pos_integer()}
  defmacrop doc_collapse(count) do
    quote do: {:doc_collapse, unquote(count)}
  end

  @typep doc_color :: {:doc_color, t, IO.ANSI.ansidata()}
  defmacrop doc_color(doc, color) do
    quote do: {:doc_color, unquote(doc), unquote(color)}
  end

  @docs [
    :doc_string,
    :doc_cons,
    :doc_nest,
    :doc_break,
    :doc_group,
    :doc_color,
    :doc_force,
    :doc_fits,
    :doc_collapse
  ]

  defguard is_doc(doc)
           when is_binary(doc) or doc in [:doc_nil, :doc_line] or
                  (is_tuple(doc) and elem(doc, 0) in @docs)

  # Elixir + Inspect.Opts conveniences

  @doc """
  Converts an Elixir term to an algebra document
  according to the `Inspect` protocol.
  """
  @spec to_doc(any, Inspect.Opts.t()) :: t
  def to_doc(term, opts)

  def to_doc(%_{} = struct, %Inspect.Opts{} = opts) do
    if opts.structs do
      try do
        Inspect.inspect(struct, opts)
      rescue
        caught_exception ->
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
                "got #{inspect(caught_exception.__struct__)} with message " <>
                  "#{inspect(Exception.message(caught_exception))} while inspecting #{res}"

              exception = Inspect.Error.exception(message: message)

              if opts.safe do
                Inspect.inspect(exception, opts)
              else
                reraise(exception, __STACKTRACE__)
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

  @doc ~S"""
  Wraps `collection` in `left` and `right` according to limit and contents.

  It uses the given `left` and `right` documents as surrounding and the
  separator document `separator` to separate items in `docs`. If all entries
  in the collection are simple documents (texts or strings), then this function
  attempts to put as much as possible on the same line. If they are not simple,
  only one entry is shown per line if they do not fit.

  The limit in the given `inspect_opts` is respected and when reached this
  function stops processing and outputs `"..."` instead.

  ## Options

    * `:separator` - the separator used between each doc
    * `:break` - If `:strict`, always break between each element. If `:flex`,
      breaks only when necessary. If `:maybe`, chooses `:flex` only if all
      elements are text-based, otherwise is `:strict`

  ## Examples

      iex> inspect_opts = %Inspect.Opts{limit: :infinity}
      iex> fun = fn i, _opts -> to_string(i) end
      iex> doc = Inspect.Algebra.container_doc("[", Enum.to_list(1..5), "]", inspect_opts, fun)
      iex> Inspect.Algebra.format(doc, 5) |> IO.iodata_to_binary()
      "[1,\n 2,\n 3,\n 4,\n 5]"

      iex> inspect_opts = %Inspect.Opts{limit: 3}
      iex> fun = fn i, _opts -> to_string(i) end
      iex> doc = Inspect.Algebra.container_doc("[", Enum.to_list(1..5), "]", inspect_opts, fun)
      iex> Inspect.Algebra.format(doc, 20) |> IO.iodata_to_binary()
      "[1, 2, 3, ...]"

      iex> inspect_opts = %Inspect.Opts{limit: 3}
      iex> fun = fn i, _opts -> to_string(i) end
      iex> opts = [separator: "!"]
      iex> doc = Inspect.Algebra.container_doc("[", Enum.to_list(1..5), "]", inspect_opts, fun, opts)
      iex> Inspect.Algebra.format(doc, 20) |> IO.iodata_to_binary()
      "[1! 2! 3! ...]"

  """
  @doc since: "1.6.0"
  @spec container_doc(t, [any], t, Inspect.Opts.t(), (term, Inspect.Opts.t() -> t), keyword()) ::
          t
  def container_doc(left, collection, right, inspect_opts, fun, opts \\ [])
      when is_doc(left) and is_list(collection) and is_doc(right) and is_function(fun, 2) and
             is_list(opts) do
    case collection do
      [] ->
        concat(left, right)

      _ ->
        break = Keyword.get(opts, :break, :maybe)
        separator = Keyword.get(opts, :separator, @container_separator)

        {docs, simple?} =
          container_each(collection, inspect_opts.limit, inspect_opts, fun, [], break == :maybe)

        flex? = simple? or break == :flex
        docs = fold_doc(docs, &join(&1, &2, flex?, separator))

        case flex? do
          true -> group(concat(concat(left, nest(docs, 1)), right))
          false -> group(glue(nest(glue(left, "", docs), 2), "", right))
        end
    end
  end

  defp container_each([], _limit, _opts, _fun, acc, simple?) do
    {:lists.reverse(acc), simple?}
  end

  defp container_each(_, 0, _opts, _fun, acc, simple?) do
    {:lists.reverse(["..." | acc]), simple?}
  end

  defp container_each([term | terms], limit, opts, fun, acc, simple?) when is_list(terms) do
    limit = decrement(limit)
    doc = fun.(term, %{opts | limit: limit})
    container_each(terms, limit, opts, fun, [doc | acc], simple? and simple?(doc))
  end

  defp container_each([left | right], limit, opts, fun, acc, simple?) do
    limit = decrement(limit)
    left = fun.(left, %{opts | limit: limit})
    right = fun.(right, %{opts | limit: limit})
    simple? = simple? and simple?(left) and simple?(right)

    doc = join(left, right, simple?, @tail_separator)
    {:lists.reverse([doc | acc]), simple?}
  end

  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1

  defp join(:doc_nil, :doc_nil, _, _), do: :doc_nil
  defp join(left, :doc_nil, _, _), do: left
  defp join(:doc_nil, right, _, _), do: right
  defp join(left, right, true, sep), do: flex_glue(concat(left, sep), right)
  defp join(left, right, false, sep), do: glue(concat(left, sep), right)

  defp simple?(doc_cons(left, right)), do: simple?(left) and simple?(right)
  defp simple?(doc_color(doc, _)), do: simple?(doc)
  defp simple?(doc_string(_, _)), do: true
  defp simple?(:doc_nil), do: true
  defp simple?(other), do: is_binary(other)

  @doc false
  @deprecated "Use a combination of concat/2 and nest/2 instead"
  def surround(left, doc, right) when is_doc(left) and is_doc(doc) and is_doc(right) do
    concat(concat(left, nest(doc, 1)), right)
  end

  @doc false
  @deprecated "Use Inspect.Algebra.container_doc/6 instead"
  def surround_many(
        left,
        docs,
        right,
        %Inspect.Opts{} = inspect,
        fun,
        separator \\ @container_separator
      )
      when is_doc(left) and is_list(docs) and is_doc(right) and is_function(fun, 2) do
    container_doc(left, docs, right, inspect, fun, separator: separator)
  end

  # Algebra API

  @doc """
  Returns a document entity used to represent nothingness.

  ## Examples

      iex> Inspect.Algebra.empty()
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
  @doc since: "1.6.0"
  @spec string(String.t()) :: doc_string
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
  @doc since: "1.4.0"
  @spec color(t, Inspect.Opts.color_key(), Inspect.Opts.t()) :: doc_color
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
  @spec nest(t, non_neg_integer | :cursor | :reset, :always | :break) :: doc_nest
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
  Returns a break document based on the given `string`.

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
  node, emitting up to `max` new lines.
  """
  @doc since: "1.6.0"
  @spec collapse_lines(pos_integer) :: doc_collapse
  def collapse_lines(max) when is_integer(max) and max > 0 do
    doc_collapse(max)
  end

  @doc """
  Considers the next break as fit.

  `mode` can be `:enabled` or `:disabled`. When `:enabled`,
  it will consider the document as fit as soon as it finds
  the next break, effectively cancelling the break. It will
  also ignore any `force_unfit/1` in search of the next break.

  When disabled, it behaves as usual and it will ignore
  any further `next_break_fits/2` instruction.

  ## Examples

  This is used by Elixir's code formatter to avoid breaking
  code at some specific locations. For example, consider this
  code:

      some_function_call(%{..., key: value, ...})

  Now imagine that this code does not fit its line. The code
  formatter introduces breaks inside `(` and `)` and inside
  `%{` and `}`. Therefore the document would break as:

      some_function_call(
        %{
          ...,
          key: value,
          ...
        }
      )

  The formatter wraps the algebra document representing the
  map in `next_break_fits/1` so the code is formatted as:

      some_function_call(%{
        ...,
        key: value,
        ...
      })

  """
  @doc since: "1.6.0"
  @spec next_break_fits(t, :enabled | :disabled) :: doc_fits
  def next_break_fits(doc, mode \\ @next_break_fits)
      when is_doc(doc) and mode in [:enabled, :disabled] do
    doc_fits(doc, mode)
  end

  @doc """
  Forces the current group to be unfit.
  """
  @doc since: "1.6.0"
  @spec force_unfit(t) :: doc_force
  def force_unfit(doc) when is_doc(doc) do
    doc_force(doc)
  end

  @doc """
  Returns a flex break document based on the given `string`.

  A flex break still causes a group to break, like `break/1`,
  but it is re-evaluated when the documented is rendered.

  For example, take a group document represented as `[1, 2, 3]`
  where the space after every comma is a break. When the document
  above does not fit a single line, all breaks are enabled,
  causing the document to be rendered as:

      [1,
       2,
       3]

  However, if flex breaks are used, then each break is re-evaluated
  when rendered, so the document could be possible rendered as:

      [1, 2,
       3]

  Hence the name "flex". they are more flexible when it comes
  to the document fitting. On the other hand, they are more expensive
  since each break needs to be re-evaluated.

  This function is used by `container_doc/4` and friends to the
  maximum number of entries on the same line.
  """
  @doc since: "1.6.0"
  @spec flex_break(binary) :: doc_break
  def flex_break(string \\ " ") when is_binary(string) do
    doc_break(string, :flex)
  end

  @doc """
  Glues two documents (`doc1` and `doc2`) inserting a
  `flex_break/1` given by `break_string` between them.

  This function is used by `container_doc/6` and friends
  to the maximum number of entries on the same line.
  """
  @doc since: "1.6.0"
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

  The group mode can also be set to `:inherit`, which means it
  automatically breaks if the parent group has broken too.

  ## Examples

      iex> doc =
      ...>   Inspect.Algebra.group(
      ...>     Inspect.Algebra.concat(
      ...>       Inspect.Algebra.group(
      ...>         Inspect.Algebra.concat(
      ...>           "Hello,",
      ...>           Inspect.Algebra.concat(
      ...>             Inspect.Algebra.break(),
      ...>             "A"
      ...>           )
      ...>         )
      ...>       ),
      ...>       Inspect.Algebra.concat(
      ...>         Inspect.Algebra.break(),
      ...>         "B"
      ...>       )
      ...>     )
      ...>   )
      iex> Inspect.Algebra.format(doc, 80)
      ["Hello,", " ", "A", " ", "B"]
      iex> Inspect.Algebra.format(doc, 6)
      ["Hello,", "\n", "A", "\n", "B"]

  """
  @spec group(t, :self | :inherit) :: doc_group
  def group(doc, mode \\ :self) when is_doc(doc) do
    doc_group(doc, mode)
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

      iex> doc =
      ...>   Inspect.Algebra.concat(
      ...>     Inspect.Algebra.concat(
      ...>       "Hughes",
      ...>       Inspect.Algebra.line()
      ...>     ),
      ...>     "Wadler"
      ...>   )
      iex> Inspect.Algebra.format(doc, 80)
      ["Hughes", "\n", "Wadler"]

  """
  @doc since: "1.6.0"
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
      iex> docs =
      ...>   Inspect.Algebra.fold_doc(docs, fn doc, acc ->
      ...>     Inspect.Algebra.concat([doc, "!", acc])
      ...>   end)
      iex> Inspect.Algebra.format(docs, 80)
      ["A", "!", "B", "!", "C"]

  """
  @spec fold_doc([t], (t, t -> t)) :: t
  def fold_doc(docs, folder_fun)

  def fold_doc([], _folder_fun), do: empty()
  def fold_doc([doc], _folder_fun), do: doc

  def fold_doc([doc | docs], folder_fun) when is_function(folder_fun, 2),
    do: folder_fun.(doc, fold_doc(docs, folder_fun))

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
  #   * flat - represents a document with breaks as flats (a break may fit, as it may break)
  #   * break - represents a document with breaks as breaks (a break always fits, since it breaks)
  #
  # The following modes are exclusive to fitting
  #
  #   * flat_no_break - represents a document with breaks as flat not allowed to enter in break mode
  #   * break_no_flat - represents a document with breaks as breaks not allowed to enter in flat mode
  #
  @typep mode :: :flat | :flat_no_break | :break

  @spec fits?(width :: integer(), column :: integer(), break? :: boolean(), entries) :: boolean()
        when entries: [{integer(), mode(), t()}] | {:tail, boolean(), entries}

  # We need at least a break to consider the document does not fit since a
  # large document without breaks has no option but fitting its current line.
  #
  # In case we have groups and the group fits, we need to consider the group
  # parent without the child breaks, hence {:tail, b?, t} below.
  defp fits?(w, k, b?, _) when k > w and b?, do: false
  defp fits?(_, _, _, []), do: true
  defp fits?(w, k, _, {:tail, b?, t}), do: fits?(w, k, b?, t)

  ## Flat no break

  defp fits?(w, k, b?, [{i, _, doc_fits(x, :disabled)} | t]),
    do: fits?(w, k, b?, [{i, :flat_no_break, x} | t])

  defp fits?(w, k, b?, [{i, :flat_no_break, doc_fits(x, _)} | t]),
    do: fits?(w, k, b?, [{i, :flat_no_break, x} | t])

  ## Breaks no flat

  defp fits?(w, k, b?, [{i, _, doc_fits(x, :enabled)} | t]),
    do: fits?(w, k, b?, [{i, :break_no_flat, x} | t])

  defp fits?(w, k, b?, [{i, :break_no_flat, doc_force(x)} | t]),
    do: fits?(w, k, b?, [{i, :break_no_flat, x} | t])

  defp fits?(_, _, _, [{_, :break_no_flat, doc_break(_, _)} | _]), do: true
  defp fits?(_, _, _, [{_, :break_no_flat, :doc_line} | _]), do: true

  ## Breaks

  defp fits?(_, _, _, [{_, :break, doc_break(_, _)} | _]), do: true
  defp fits?(_, _, _, [{_, :break, :doc_line} | _]), do: true

  defp fits?(w, k, b?, [{i, :break, doc_group(x, _)} | t]),
    do: fits?(w, k, b?, [{i, :flat, x} | {:tail, b?, t}])

  ## Catch all

  defp fits?(w, _, _, [{i, _, :doc_line} | t]), do: fits?(w, i, false, t)
  defp fits?(w, k, b?, [{_, _, :doc_nil} | t]), do: fits?(w, k, b?, t)
  defp fits?(w, _, b?, [{i, _, doc_collapse(_)} | t]), do: fits?(w, i, b?, t)
  defp fits?(w, k, b?, [{i, m, doc_color(x, _)} | t]), do: fits?(w, k, b?, [{i, m, x} | t])
  defp fits?(w, k, b?, [{_, _, doc_string(_, l)} | t]), do: fits?(w, k + l, b?, t)
  defp fits?(w, k, b?, [{_, _, s} | t]) when is_binary(s), do: fits?(w, k + byte_size(s), b?, t)
  defp fits?(_, _, _, [{_, _, doc_force(_)} | _]), do: false
  defp fits?(w, k, _, [{_, _, doc_break(s, _)} | t]), do: fits?(w, k + byte_size(s), true, t)
  defp fits?(w, k, b?, [{i, m, doc_nest(x, _, :break)} | t]), do: fits?(w, k, b?, [{i, m, x} | t])

  defp fits?(w, k, b?, [{i, m, doc_nest(x, j, _)} | t]),
    do: fits?(w, k, b?, [{apply_nesting(i, k, j), m, x} | t])

  defp fits?(w, k, b?, [{i, m, doc_cons(x, y)} | t]),
    do: fits?(w, k, b?, [{i, m, x}, {i, m, y} | t])

  defp fits?(w, k, b?, [{i, m, doc_group(x, _)} | t]),
    do: fits?(w, k, b?, [{i, m, x} | {:tail, b?, t}])

  @spec format(integer | :infinity, integer, [{integer, mode, t}]) :: [binary]
  defp format(_, _, []), do: []
  defp format(w, k, [{_, _, :doc_nil} | t]), do: format(w, k, t)
  defp format(w, _, [{i, _, :doc_line} | t]), do: [indent(i) | format(w, i, t)]
  defp format(w, k, [{i, m, doc_cons(x, y)} | t]), do: format(w, k, [{i, m, x}, {i, m, y} | t])
  defp format(w, k, [{i, m, doc_color(x, c)} | t]), do: [ansi(c) | format(w, k, [{i, m, x} | t])]
  defp format(w, k, [{_, _, doc_string(s, l)} | t]), do: [s | format(w, k + l, t)]
  defp format(w, k, [{_, _, s} | t]) when is_binary(s), do: [s | format(w, k + byte_size(s), t)]
  defp format(w, k, [{i, m, doc_force(x)} | t]), do: format(w, k, [{i, m, x} | t])
  defp format(w, k, [{i, m, doc_fits(x, _)} | t]), do: format(w, k, [{i, m, x} | t])
  defp format(w, _, [{i, _, doc_collapse(max)} | t]), do: collapse(format(w, i, t), max, 0, i)

  # Flex breaks are not conditional to the mode
  defp format(w, k, [{i, m, doc_break(s, :flex)} | t]) do
    k = k + byte_size(s)

    if w == :infinity or m == :flat or fits?(w, k, true, t) do
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
  defp format(w, k, [{i, :break, doc_group(x, :inherit)} | t]) do
    format(w, k, [{i, :break, x} | t])
  end

  defp format(w, k, [{i, _, doc_group(x, _)} | t]) do
    if w == :infinity or fits?(w, k, false, [{i, :flat, x}]) do
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
