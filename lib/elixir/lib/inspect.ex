import Kernel, except: [inspect: 1]
import Inspect.Algebra

alias Code.Identifier

defprotocol Inspect do
  @moduledoc """
  The `Inspect` protocol converts an Elixir data structure into an
  algebra document.

  This is typically done when you want to customize how your own
  structs are inspected in logs and the terminal.

  This documentation refers to implementing the `Inspect` protocol
  for your own data structures. To learn more about using inspect,
  see `Kernel.inspect/2` and `IO.inspect/2`.

  ## Inspect representation

  There are typically three choices of inspect representation. In order
  to understand them, let's imagine we have the following `User` struct:

      defmodule User do
        defstruct [:id, :name, :address]
      end

  Our choices are:

    1. Print the struct using Elixir's struct syntax, for example:
       `%User{address: "Earth", id: 13, name: "Jane"}`. This is the
       default representation and best choice if all struct fields
       are public.

    2. Print using the `#User<...>` notation, for example: `#User<id: 13, name: "Jane", ...>`.
       This notation does not emit valid Elixir code and is typically
       used when the struct has private fields (for example, you may want
       to hide the field `:address` to redact person identifiable information).

    3. Print the struct using the expression syntax, for example:
       `User.new(13, "Jane", "Earth")`. This assumes there is a `User.new/3`
       function. This option is mostly used as an alternative to option 2
       for representing custom data structures, such as `MapSet`, `Date.Range`,
       and others.

  You can implement the Inspect protocol for your own structs while
  adhering to the conventions above. Option 1 is the default representation
  and you can quickly achieve option 2 by deriving the `Inspect` protocol.
  For option 3, you need your custom implementation.

  ## Deriving

  The `Inspect` protocol can be derived to customize the order of fields
  (the default is alphabetical) and hide certain fields from structs,
  so they don't show up in logs, inspects and similar. The latter is
  especially useful for fields containing private information.

  The supported options are:

    * `:only` - only include the given fields when inspecting.

    * `:except` - remove the given fields when inspecting.

    * `:optional` - (since v1.14.0) do not include a field if it
      matches its default value. This can be used to simplify the
      struct representation at the cost of hiding information.

  Whenever `:only` or `:except` are used to restrict fields,
  the struct will be printed using the `#User<...>` notation,
  as the struct can no longer be copy and pasted as valid Elixir
  code. Let's see an example:

      defmodule User do
        @derive {Inspect, only: [:id, :name]}
        defstruct [:id, :name, :address]
      end

      inspect(%User{id: 1, name: "Jane", address: "Earth"})
      #=> #User<id: 1, name: "Jane", ...>

  If you use only the `:optional` option, the struct will still be
  printed as `%User{...}`.

  ## Custom implementation

  You can also define your custom protocol implementation by
  defining the `inspect/2` function. The function receives the
  entity to be inspected followed by the inspecting options,
  represented by the struct `Inspect.Opts`. Building of the
  algebra document is done with `Inspect.Algebra`.

  Many times, inspecting a structure can be implemented in function
  of existing entities. For example, here is `MapSet`'s `inspect/2`
  implementation:

      defimpl Inspect, for: MapSet do
        import Inspect.Algebra

        def inspect(map_set, opts) do
          concat(["MapSet.new(", Inspect.List.inspect(MapSet.to_list(map_set), opts), ")"])
        end
      end

  The [`concat/1`](`Inspect.Algebra.concat/1`) function comes from
  `Inspect.Algebra` and it concatenates algebra documents together.
  In the example above it is concatenating the string `"MapSet.new("`,
  the document returned by `Inspect.Algebra.to_doc/2`, and the final
  string `")"`. Therefore, the MapSet with the numbers 1, 2, and 3
  will be printed as:

      iex> MapSet.new([1, 2, 3], fn x -> x * 2 end)
      MapSet.new([2, 4, 6])

  In other words, `MapSet`'s inspect representation returns an expression
  that, when evaluated, builds the `MapSet` itself.

  ### Error handling

  In case there is an error while your structure is being inspected,
  Elixir will raise an `ArgumentError` error and will automatically fall back
  to a raw representation for printing the structure. Furthermore, you
  must be careful when debugging your own Inspect implementation, as calls
  to `IO.inspect/2` or `dbg/1` may trigger an infinite loop (as in order to
  inspect/debug the data structure, you must call `inspect` itself).

  Here are some tips:

    * For debugging, use `IO.inspect/2` with the `structs: false` option,
      which disables custom printing and avoids calling the Inspect
      implementation recursively

    * To access the underlying error on your custom `Inspect` implementation,
      you may invoke the protocol directly. For example, we could invoke the
      `Inspect.MapSet` implementation above as:

          Inspect.MapSet.inspect(MapSet.new(), %Inspect.Opts{})

  """

  # Handle structs in Any
  @fallback_to_any true

  @doc """
  Converts `term` into an algebra document.

  This function shouldn't be invoked directly, unless when implementing
  a custom `inspect_fun` to be given to `Inspect.Opts`. Everywhere else,
  `Inspect.Algebra.to_doc/2` should be preferred as it handles structs
  and exceptions.
  """
  @spec inspect(t, Inspect.Opts.t()) :: Inspect.Algebra.t()
  def inspect(term, opts)
end

defimpl Inspect, for: Atom do
  require Macro

  def inspect(atom, opts) do
    color(Macro.inspect_atom(:literal, atom), color_key(atom), opts)
  end

  defp color_key(atom) when is_boolean(atom), do: :boolean
  defp color_key(nil), do: nil
  defp color_key(_), do: :atom
end

defimpl Inspect, for: BitString do
  def inspect(term, opts) when is_binary(term) do
    %Inspect.Opts{binaries: bins, base: base, printable_limit: printable_limit} = opts

    if bins == :as_strings or
         (bins == :infer and String.printable?(term, printable_limit) and base == :decimal) do
      inspected =
        case Identifier.escape(term, ?", printable_limit) do
          {escaped, ""} -> [?", escaped, ?"]
          {escaped, _} -> [?", escaped, ?", " <> ..."]
        end

      color(IO.iodata_to_binary(inspected), :string, opts)
    else
      inspect_bitstring(term, opts)
    end
  end

  def inspect(term, opts) do
    inspect_bitstring(term, opts)
  end

  defp inspect_bitstring("", opts) do
    color("<<>>", :binary, opts)
  end

  defp inspect_bitstring(bitstring, opts) do
    left = color("<<", :binary, opts)
    right = color(">>", :binary, opts)
    inner = each_bit(bitstring, opts.limit, opts)
    group(concat(concat(left, nest(inner, 2)), right))
  end

  defp each_bit(_, 0, _) do
    "..."
  end

  defp each_bit(<<>>, _counter, _opts) do
    :doc_nil
  end

  defp each_bit(<<h::8>>, _counter, opts) do
    Inspect.Integer.inspect(h, opts)
  end

  defp each_bit(<<h, t::bitstring>>, counter, opts) do
    flex_glue(
      concat(Inspect.Integer.inspect(h, opts), ","),
      each_bit(t, decrement(counter), opts)
    )
  end

  defp each_bit(bitstring, _counter, opts) do
    size = bit_size(bitstring)
    <<h::size(^size)>> = bitstring
    concat(Inspect.Integer.inspect(h, opts), "::size(" <> Integer.to_string(size) <> ")")
  end

  @compile {:inline, decrement: 1}
  defp decrement(:infinity), do: :infinity
  defp decrement(counter), do: counter - 1
end

defimpl Inspect, for: List do
  def inspect([], opts) do
    color("[]", :list, opts)
  end

  # TODO: Remove :char_list and :as_char_lists handling on v2.0
  def inspect(term, opts) do
    %Inspect.Opts{
      charlists: lists,
      char_lists: lists_deprecated,
      printable_limit: printable_limit
    } = opts

    lists =
      if lists == :infer and lists_deprecated != :infer do
        case lists_deprecated do
          :as_char_lists ->
            IO.warn(
              "the :char_lists inspect option and its :as_char_lists " <>
                "value are deprecated, use the :charlists option and its " <>
                ":as_charlists value instead"
            )

            :as_charlists

          _ ->
            IO.warn("the :char_lists inspect option is deprecated, use :charlists instead")
            lists_deprecated
        end
      else
        lists
      end

    open = color("[", :list, opts)
    sep = color(",", :list, opts)
    close = color("]", :list, opts)

    cond do
      lists == :as_charlists or (lists == :infer and List.ascii_printable?(term, printable_limit)) ->
        inspected =
          case Identifier.escape(IO.chardata_to_string(term), ?", printable_limit) do
            {escaped, ""} -> [?~, ?c, ?", escaped, ?"]
            {escaped, _} -> [?~, ?c, ?", escaped, ?", " ++ ..."]
          end

        color(IO.iodata_to_binary(inspected), :charlist, opts)

      keyword?(term) ->
        container_doc(open, term, close, opts, &keyword/2, separator: sep, break: :strict)

      true ->
        container_doc(open, term, close, opts, &to_doc/2, separator: sep)
    end
  end

  @doc false
  def keyword({key, value}, opts) do
    key = color(Macro.inspect_atom(:key, key), :atom, opts)
    concat(key, concat(" ", to_doc(value, opts)))
  end

  @doc false
  def keyword?([{key, _value} | rest]) when is_atom(key) do
    case Atom.to_charlist(key) do
      [?E, ?l, ?i, ?x, ?i, ?r, ?.] ++ _ -> false
      _ -> keyword?(rest)
    end
  end

  def keyword?([]), do: true
  def keyword?(_other), do: false
end

defimpl Inspect, for: Tuple do
  def inspect(tuple, opts) do
    open = color("{", :tuple, opts)
    sep = color(",", :tuple, opts)
    close = color("}", :tuple, opts)
    container_opts = [separator: sep, break: :flex]
    container_doc(open, Tuple.to_list(tuple), close, opts, &to_doc/2, container_opts)
  end
end

defimpl Inspect, for: Map do
  def inspect(map, opts) do
    list =
      if Keyword.get(opts.custom_options, :sort_maps) do
        map |> Map.to_list() |> :lists.sort()
      else
        Map.to_list(map)
      end

    fun =
      if Inspect.List.keyword?(list) do
        &Inspect.List.keyword/2
      else
        sep = color(" => ", :map, opts)
        &to_assoc(&1, &2, sep)
      end

    map_container_doc(list, "", opts, fun)
  end

  def inspect(map, name, infos, opts) do
    fun = fn %{field: field}, opts -> Inspect.List.keyword({field, Map.get(map, field)}, opts) end
    map_container_doc(infos, name, opts, fun)
  end

  defp to_assoc({key, value}, opts, sep) do
    concat(concat(to_doc(key, opts), sep), to_doc(value, opts))
  end

  defp map_container_doc(list, name, opts, fun) do
    open = color("%" <> name <> "{", :map, opts)
    sep = color(",", :map, opts)
    close = color("}", :map, opts)
    container_doc(open, list, close, opts, fun, separator: sep, break: :strict)
  end
end

defimpl Inspect, for: Integer do
  def inspect(term, %Inspect.Opts{base: base} = opts) do
    inspected = Integer.to_string(term, base_to_value(base)) |> prepend_prefix(base)
    color(inspected, :number, opts)
  end

  defp base_to_value(base) do
    case base do
      :binary -> 2
      :decimal -> 10
      :octal -> 8
      :hex -> 16
    end
  end

  defp prepend_prefix(value, :decimal), do: value

  defp prepend_prefix(<<?-, value::binary>>, base) do
    "-" <> prepend_prefix(value, base)
  end

  defp prepend_prefix(value, base) do
    prefix =
      case base do
        :binary -> "0b"
        :octal -> "0o"
        :hex -> "0x"
      end

    prefix <> value
  end
end

defimpl Inspect, for: Float do
  def inspect(float, opts) do
    abs = abs(float)

    formatted =
      if abs >= 1.0 and abs < 1.0e16 and trunc(float) == float do
        [Integer.to_string(trunc(float)), ?., ?0]
      else
        Float.to_charlist(float)
      end

    color(IO.iodata_to_binary(formatted), :number, opts)
  end
end

defimpl Inspect, for: Regex do
  def inspect(regex = %{opts: regex_opts}, opts) when is_list(regex_opts) do
    case translate_options(regex_opts, []) do
      :error ->
        concat([
          "Regex.compile!(",
          Inspect.BitString.inspect(regex.source, opts),
          ", ",
          Inspect.List.inspect(regex_opts, opts),
          ")"
        ])

      translated_opts ->
        {escaped, _} =
          regex.source
          |> normalize(<<>>)
          |> Identifier.escape(?/, :infinity, &escape_map/1)

        source = IO.iodata_to_binary([?~, ?r, ?/, escaped, ?/, translated_opts])
        color(source, :regex, opts)
    end
  end

  defp translate_options([:dotall, {:newline, :anycrlf} | t], acc),
    do: translate_options(t, [?s | acc])

  defp translate_options([:unicode, :ucp | t], acc), do: translate_options(t, [?u | acc])
  defp translate_options([:caseless | t], acc), do: translate_options(t, [?i | acc])
  defp translate_options([:extended | t], acc), do: translate_options(t, [?x | acc])
  defp translate_options([:firstline | t], acc), do: translate_options(t, [?f | acc])
  defp translate_options([:ungreedy | t], acc), do: translate_options(t, [?U | acc])
  defp translate_options([:multiline | t], acc), do: translate_options(t, [?m | acc])
  defp translate_options([], acc), do: acc
  defp translate_options(_t, _acc), do: :error

  defp normalize(<<?\\, ?\\, rest::binary>>, acc), do: normalize(rest, <<acc::binary, ?\\, ?\\>>)
  defp normalize(<<?\\, ?/, rest::binary>>, acc), do: normalize(rest, <<acc::binary, ?/>>)
  defp normalize(<<?\\, ?#, ?{, rest::binary>>, acc), do: normalize(rest, <<acc::binary, ?#, ?{>>)
  defp normalize(<<char, rest::binary>>, acc), do: normalize(rest, <<acc::binary, char>>)
  defp normalize(<<>>, acc), do: acc

  defp escape_map(?\a), do: [?\\, ?a]
  defp escape_map(?\f), do: [?\\, ?f]
  defp escape_map(?\n), do: [?\\, ?n]
  defp escape_map(?\r), do: [?\\, ?r]
  defp escape_map(?\t), do: [?\\, ?t]
  defp escape_map(?\v), do: [?\\, ?v]
  defp escape_map(_), do: false
end

defimpl Inspect, for: Function do
  @elixir_compiler :binary.bin_to_list("elixir_compiler_")

  def inspect(function, _opts) do
    fun_info = Function.info(function)
    mod = fun_info[:module]
    name = fun_info[:name]

    cond do
      not is_atom(mod) ->
        "#Function<#{uniq(fun_info)}/#{fun_info[:arity]}>"

      fun_info[:type] == :external and fun_info[:env] == [] ->
        inspected_as_atom = Macro.inspect_atom(:literal, mod)
        inspected_as_function = Macro.inspect_atom(:remote_call, name)
        "&#{inspected_as_atom}.#{inspected_as_function}/#{fun_info[:arity]}"

      match?(@elixir_compiler ++ _, Atom.to_charlist(mod)) ->
        if function_exported?(mod, :__RELATIVE__, 0) do
          "#Function<#{uniq(fun_info)} in file:#{mod.__RELATIVE__()}>"
        else
          default_inspect(mod, fun_info)
        end

      true ->
        default_inspect(mod, fun_info)
    end
  end

  defp default_inspect(mod, fun_info) do
    inspected_as_atom = Macro.inspect_atom(:literal, mod)
    extracted_name = extract_name(fun_info[:name])
    "#Function<#{uniq(fun_info)}/#{fun_info[:arity]} in #{inspected_as_atom}#{extracted_name}>"
  end

  defp extract_name([]) do
    ""
  end

  defp extract_name(name) do
    case Identifier.extract_anonymous_fun_parent(name) do
      {name, arity} ->
        "." <> Macro.inspect_atom(:remote_call, name) <> "/" <> arity

      :error ->
        "." <> Macro.inspect_atom(:remote_call, name)
    end
  end

  defp uniq(fun_info) do
    Integer.to_string(fun_info[:new_index]) <> "." <> Integer.to_string(fun_info[:uniq])
  end
end

defimpl Inspect, for: Inspect.Error do
  @impl true
  def inspect(%{stacktrace: stacktrace} = inspect_error, _opts) do
    message = Exception.message(inspect_error)
    format_output(message, stacktrace)
  end

  defp format_output(message, [_ | _] = stacktrace) do
    stacktrace = Exception.format_stacktrace(stacktrace)

    """
    #Inspect.Error<
    #{Inspect.Error.pad(message, 2)}

      Stacktrace:

    #{stacktrace}
    >\
    """
  end

  defp format_output(message, []) do
    """
    #Inspect.Error<
      #{Inspect.Error.pad(message, 2)}
    >\
    """
  end
end

defimpl Inspect, for: PID do
  def inspect(pid, _opts) do
    "#PID" <> IO.iodata_to_binary(:erlang.pid_to_list(pid))
  end
end

defimpl Inspect, for: Port do
  def inspect(port, _opts) do
    IO.iodata_to_binary(:erlang.port_to_list(port))
  end
end

defimpl Inspect, for: Reference do
  def inspect(ref, _opts) do
    [?#, ?R, ?e, ?f] ++ rest = :erlang.ref_to_list(ref)
    "#Reference" <> IO.iodata_to_binary(rest)
  end
end

defimpl Inspect, for: Any do
  defmacro __deriving__(module, struct, options) do
    fields = Enum.sort(Map.keys(struct) -- [:__exception__, :__struct__])

    only = Keyword.get(options, :only, fields)
    except = Keyword.get(options, :except, [])
    optional = Keyword.get(options, :optional, [])

    :ok = validate_option(:only, only, fields, module)
    :ok = validate_option(:except, except, fields, module)
    :ok = validate_option(:optional, optional, fields, module)

    inspect_module =
      if fields == Enum.sort(only) and except == [] do
        Inspect.Map
      else
        Inspect.Any
      end

    filtered_fields =
      fields
      |> Enum.reject(&(&1 in except))
      |> Enum.filter(&(&1 in only))

    optional? =
      if optional == [] do
        false
      else
        optional_map = for field <- optional, into: %{}, do: {field, Map.fetch!(struct, field)}

        quote do
          case unquote(Macro.escape(optional_map)) do
            %{^var!(field) => var!(default)} ->
              var!(default) == Map.get(var!(struct), var!(field))

            %{} ->
              false
          end
        end
      end

    quote do
      defimpl Inspect, for: unquote(module) do
        def inspect(var!(struct), var!(opts)) do
          var!(infos) =
            for %{field: var!(field)} = var!(info) <- unquote(module).__info__(:struct),
                var!(field) in unquote(filtered_fields) and not unquote(optional?),
                do: var!(info)

          var!(name) = Macro.inspect_atom(:literal, unquote(module))
          unquote(inspect_module).inspect(var!(struct), var!(name), var!(infos), var!(opts))
        end
      end
    end
  end

  defp validate_option(option, option_list, fields, module) do
    case option_list -- fields do
      [] ->
        :ok

      unknown_fields ->
        raise ArgumentError,
              "unknown fields #{Kernel.inspect(unknown_fields)} in #{Kernel.inspect(option)} " <>
                "when deriving the Inspect protocol for #{Kernel.inspect(module)}"
    end
  end

  def inspect(%module{} = struct, opts) do
    try do
      {module.__struct__(), module.__info__(:struct)}
    rescue
      _ -> Inspect.Map.inspect(struct, opts)
    else
      {dunder, fields} ->
        if Map.keys(dunder) == Map.keys(struct) do
          infos =
            for %{field: field} = info <- fields,
                field not in [:__struct__, :__exception__],
                do: info

          Inspect.Map.inspect(struct, Macro.inspect_atom(:literal, module), infos, opts)
        else
          Inspect.Map.inspect(struct, opts)
        end
    end
  end

  def inspect(map, name, infos, opts) do
    open = color("#" <> name <> "<", :map, opts)
    sep = color(",", :map, opts)
    close = color(">", :map, opts)

    fun = fn
      %{field: field}, opts -> Inspect.List.keyword({field, Map.get(map, field)}, opts)
      :..., _opts -> "..."
    end

    container_doc(open, infos ++ [:...], close, opts, fun, separator: sep, break: :strict)
  end
end

require Protocol

Protocol.derive(
  Inspect,
  Macro.Env,
  only: [
    :module,
    :file,
    :line,
    :function,
    :context,
    :aliases,
    :requires,
    :functions,
    :macros,
    :macro_aliases,
    :context_modules,
    :lexical_tracker
  ]
)
