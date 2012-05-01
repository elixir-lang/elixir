defmodule Record do
  # Extract record information from an Erlang file and
  # return the fields as a list of tuples.
  #
  # ## Examples
  #
  #     defrecord FileInfo, Record.extract(:file_info, from_lib: "kernel/include/file.hrl")
  #
  def extract(name, opts) do
    Record.Extractor.retrieve(name, opts)
  end

  # Main entry point for records definition.
  # This is invoked directly by Elixir.Builtin.defrecord.
  def defrecord(name, values, opts) do
    block      = Keyword.get(opts, :do)
    definition = Keyword.get(opts, :definition, Record.Definition)

    quote do
      defmodule unquote(name) do
        Record.define_functions(__MODULE__, unquote(values), unquote(definition))
        unquote(block)
      end
    end
  end

  # Private endpoint that defines the functions for the Record.
  def define_functions(module, values, definition) do
    # Escape the values so they are valid syntax nodes
    values = Macro.escape(values)

    contents = [
      reflection(module, values),
      getters_and_setters(values, 1, [], definition),
      initializers(values)
    ]

    Module.eval_quoted module, contents, [], file: __FILE__, line: __LINE__
  end

  # Define __record__/1 and __record__/2 as reflection functions
  # that returns the record names and fields.
  #
  # ## Examples
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  #     FileInfo.__record__(:name)   #=> FileInfo
  #     FileInfo.__record__(:fields) #=> [atime: nil, mtime: nil]
  #
  defp reflection(name, values) do
    quote do
      def __record__(kind),       do: __record__(kind, nil)
      def __record__(:name, _),   do: unquote(name)
      def __record__(:fields, _), do: unquote(values)
    end
  end

  # Define initializers methods. For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define three methods:
  #
  #     def new() do
  #       new([])
  #     end
  #
  #     def new([]) do
  #       { FileInfo, nil, nil }
  #     end
  #
  #     def new(opts) do
  #       { FileInfo, Keyword.get(opts, :atime), Keyword.get(opts, :mtime) }
  #     end
  #
  defp initializers(values) do
    defaults = Enum.map values, elem(&1, 2)

    # For each value, define a piece of code that will receive
    # an ordered dict of options (opts) and it will try to fetch
    # the given key from the ordered dict, falling back to the
    # default value if one does not exist.
    selective = Enum.map values, fn({k,v}) ->
      quote do: Keyword.get(opts, unquote(k), unquote(v))
    end

    quote do
      def new(), do: new([])
      def new([]), do: { __MODULE__, unquote_splicing(defaults) }
      def new(opts) when is_list(opts), do: { __MODULE__, unquote_splicing(selective) }
      def new(tuple) when is_tuple(tuple), do: setelem(tuple, 1, __MODULE__)
    end
  end

  # Implement getters and setters for each attribute.
  # For a declaration like:
  #
  #     defrecord FileInfo, atime: nil, mtime: nil
  #
  # It will define four methods:
  #
  #     def :atime.(record) do
  #       elem(record, 2)
  #     end
  #
  #     def :atime.(record, value) do
  #       setelem(record, 2, value)
  #     end
  #
  #     def :mtime.(record) do
  #       elem(record, 3)
  #     end
  #
  #     def :mtime.(record, value) do
  #       setelem(record, value, 3)
  #     end
  #
  # `element` and `setelement` will simply get and set values
  # from the record tuple. Notice that `:atime.(record)` is just
  # a dynamic way to say `atime(record)`. We need to use this
  # syntax as `unquote(key)(record)` wouldn't be valid (as Elixir
  # allows you to parenthesis just on specific cases as `foo()`
  # and `foo.bar()`)
  defp getters_and_setters([{ key, default }|t], i, acc, definition) do
    i = i + 1
    functions = definition.functions_for(key, default, i)
    getters_and_setters(t, i, [functions | acc], definition)
  end

  defp getters_and_setters([], _i, acc, _), do: acc
end

# Module responsible for extracting record definitions
# from Erlang files.
defmodule Record.Extractor do
  # Retrieve a record definition from an Erlang file using
  # the same lookup as the *include* attribute from Erlang modules.
  def retrieve(name, from: string) do
    file = to_char_list(string)

    case Erlang.code.where_is_file(file) do
    match: :non_existing
      realfile = file
    match: realfile
    end

    retrieve_record(name, realfile)
  end

  # Retrieve a record definition from an Erlang file using
  # the same lookup as the *include_lib* attribute from Erlang modules.
  def retrieve(name, from_lib: file) do
    [app|path] = Erlang.filename.split(to_char_list(file))
    case Erlang.code.lib_dir(to_char_list(app)) do
    match: { :error, _ }
      raise ArgumentError, "Lib file #{to_binary(file)} could not be found"
    match: libpath
      retrieve_record name, Erlang.filename.join([libpath|path])
    end
  end

  # Retrieve the record with the given name from the given file
  defp retrieve_record(name, file) do
    records = retrieve_from_file(file)
    if record = List.keyfind(records, name, 1) do
      parse_record(record)
    else:
      raise ArgumentError, "No record #{name} found at #{to_binary(file)}"
    end
  end

  # Parse the given file and retrieve all existent records.
  defp retrieve_from_file(file) do
    lc { :attribute, _, :record, record } in read_file(file), do: record
  end

  # Read a file and return its abstract syntax form that also
  # includes record and other preprocessor modules. This is done
  # by using Erlang's epp_dodger.
  defp read_file(file) do
    case Erlang.epp_dodger.quick_parse_file(file) do
    match: { :ok, form }
      form
    match: other
      raise "Error parsing file #{to_binary(file)}, got: #{inspect(other)}"
    end
  end

  # Parse a tuple with name and fields and returns a
  # list of second order tuples where the first element
  # is the field and the second is its default value.
  defp parse_record({ _name, fields }) do
    cons = List.foldr fields, { nil, 0 }, fn(f, acc) ->
      { :cons, 0, parse_field(f), acc }
    end
    { :value, list, _ } = Erlang.erl_eval.expr(cons, [])
    list
  end

  defp parse_field({ :typed_record_field, record_field, _type }) do
    parse_field(record_field)
  end

  defp parse_field({ :record_field, _, key }) do
    { :tuple, 0, [key, {:atom, 0, :nil}] }
  end

  defp parse_field({ :record_field, _, key, value }) do
    { :tuple, 0, [key, value] }
  end
end

# Module responsible for defining functions for each field.
defmodule Record.Definition do
  # Main entry point. It defines both default functions
  # via `default_for` and extensions via `extension_for`.
  def functions_for(key, default, i) do
    [
      default_for(key, default, i),
      extension_for(key, default, i)
    ]
  end

  # Define the default functions for each field.
  def default_for(key, _default, i) do
    bin_update = "update_" <> atom_to_binary(key)
    update     = binary_to_atom(bin_update)

    quote do
      def unquote(key).(record) do
        :erlang.element(unquote(i), record)
      end

      def unquote(key).(value, record) do
        :erlang.setelement(unquote(i), record, value)
      end

      def unquote(update).(function, record) do
        current = :erlang.element(unquote(i), record)
        :erlang.setelement(unquote(i), record, function.(current))
      end
    end
  end

  # Define extensions based on the default type.
  def extension_for(key, default, i) when is_list(default) do
    bin_key = atom_to_binary(key)
    prepend = :"prepend_#{bin_key}"
    merge   = :"merge_#{bin_key}"

    quote do
      def unquote(prepend).(value, record) do
        current = :erlang.element(unquote(i), record)
        :erlang.setelement(unquote(i), record, List.prepend(value, current))
      end

      def unquote(merge).(value, record) do
        current = :erlang.element(unquote(i), record)
        :erlang.setelement(unquote(i), record, Keyword.merge(current, value))
      end
    end
  end

  def extension_for(key, default, i) when is_number(default) do
    bin_key   = atom_to_binary(key)
    increment = :"increment_#{bin_key}"

    quote do
      def unquote(increment).(value // 1, record) do
        current = :erlang.element(unquote(i), record)
        :erlang.setelement(unquote(i), record, current + value)
      end
    end
  end

  def extension_for(_, _, _), do: nil
end