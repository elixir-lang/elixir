defmodule Record do
  # Main entry point for records definition.
  def defrecord(name, values, opts) do
    block    = Orddict.get(opts, :do)
    as       = Orddict.get(opts, :as, true)
    extensor = Orddict.get(opts, :extensor, Record::Extensor)

    quote do
      defmodule unquote(name) do
        require ::Record
        Record.getters_and_setters(unquote(values), 1, [], unquote(extensor))
        Record.initializers(unquote(values))
        unquote(block)
      end

      require unquote(name), as: unquote(as)
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
  #       { FileInfo, Orddict.get(opts, :atime), Orddict.get(opts, :mtime) }
  #     end
  #
  defmacro initializers(values) do
    defaults = Enum.map values, elem(_, 2)

    # For each value, define a piece of code that will receive
    # an ordered dict of options (opts) and it will try to fetch
    # the given key from the ordered dict, falling back to the
    # default value if one does not exist.
    selective = Enum.map values, fn({k,v}) {
      quote { ::Orddict.get(opts, unquote(k), unquote(v)) }
    }

    quote do
      def new(), do: new([])
      def new([]), do: { __MODULE__, unquote_splicing(defaults) }
      def new(opts), do: { __MODULE__, unquote_splicing(selective) }
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
  #       element(2, record)
  #     end
  #
  #     def :atime.(record, value) do
  #       setelement(2, value, record)
  #     end
  #
  #     def :mtime.(record) do
  #       element(3, record)
  #     end
  #
  #     def :mtime.(record, value) do
  #       setelement(3, value, record)
  #     end
  #
  # `element` and `setelement` will simply get and set values
  # from the record tuple. Notice that `:atime.(record)` is just
  # a dynamic way to say `atime(record)`. We need to use this
  # syntax as `unquote(key)(record)` wouldn't be valid (as Elixir
  # allows you to parenthesis just on specific cases as `foo()`
  # and `foo.bar()`)
  defmacro getters_and_setters([{ key, default }|t], i, acc, extensor) do
    i = i + 1

    contents = quote do
      def unquote(key).(record) do
        element(unquote(i), record)
      end

      def unquote(key).(value, record) do
        setelement(unquote(i), record, value)
      end
    end

    typed = extensor.extension_for(key, default, i)
    getters_and_setters(t, i, [contents, typed | acc], extensor)
  end

  defmacro getters_and_setters([], _i, acc, _), do: acc
end

# Provides default extensions for a regular record.
# It adds append_, prepend_ and merge_ helpers for lists
# and increment_ for numbers.
defmodule Record::Extensor do
  def extension_for(key, default, i) when is_list(default) do
    bin_key = atom_to_binary(key, :utf8)
    append  = :"append_#{bin_key}"
    prepend = :"prepend_#{bin_key}"
    merge   = :"merge_#{bin_key}"

    quote do
      def unquote(append).(value, record) do
        current = element(unquote(i), record)
        setelement(unquote(i), record, List.append(current, value))
      end

      def unquote(prepend).(value, record) do
        current = element(unquote(i), record)
        setelement(unquote(i), record, List.append(value, current))
      end

      def unquote(merge).(value, record) do
        current = element(unquote(i), record)
        setelement(unquote(i), record, Orddict.merge(current, value))
      end
    end
  end

  def extension_for(key, default, i) when is_number(default) do
    bin_key   = atom_to_binary(key, :utf8)
    increment = :"increment_#{bin_key}"

    quote do
      def unquote(increment).(value // 1, record) do
        current = element(unquote(i), record)
        setelement(unquote(i), record, current + value)
      end
    end
  end

  def extension_for(_, _, _), do: nil
end