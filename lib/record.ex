module Record

defmacro define(name, values) do
  functions = getters_and_setters(values, 1, [])
  functions = [initializers(name, values)|functions]

  quote do
    module unquote(name), do: unquote(functions)
  end
end

private

def initializers(name, values) do
  defaults  = Orddict.values(values)

  selective = List.map values, fn({k,v}) {
    quote { Orddict.fetch(opts, unquote(k), unquote(v)) }
  }

  quote do
    def new(), do: new([])
    def new([]), do: { unquote(name), unquote_splice(defaults) }
    def new(opts), do: { unquote(name), unquote_splice(selective) }
  end
end

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