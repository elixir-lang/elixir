module Record

# Main entry point for records definition.
defmacro define(_parent, name, values) do
  functions = getters_and_setters(values, 1, [])
  functions = [initializers(name, values)|functions]

  quote do
    # Use `module NAME, do: CONTENTS` syntax which is
    # the same as `module NAME do CONTENTS end`. We need
    # to wrap this in a block so this module declaration
    # do not affect the outer module one.
    module unquote(name), do: unquote(functions)
  end
end

private

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
#       { FileInfo, Orddict.fetch(opts, :atime, nil), Orddict.fetch(opts, :mtime, nil) }
#     end
#
def initializers(name, values) do
  # Get default values from the dictionary.
  defaults  = Orddict.values(values)

  # For each value, define a piece of code that will receive
  # an ordered dict of options (opts) and it will try to fetch
  # the given key from the ordered dict, falling back to the
  # default value if one does not exist.
  selective = List.map values, fn({k,v}) {
    quote { Orddict.fetch(opts, unquote(k), unquote(v)) }
  }

  quote do
    def new(), do: new([])
    def new([]), do: { unquote(name), unquote_splice(defaults) }
    def new(opts), do: { unquote(name), unquote_splice(selective) }
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
def getters_and_setters([{ key, _ }|t], i, acc) do
  i = i + 1

  contents = quote do
    def unquote(key).(record) do
      element(unquote(i), record)
    end

    def unquote(key).(record, value) do
      setelement(unquote(i), value, record)
    end
  end

  getters_and_setters(t, i, [contents|acc])
end

def getters_and_setters([], _i, acc), do: acc