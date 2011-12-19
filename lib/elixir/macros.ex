ns Elixir::Macros

defmacro unless: [clause, options] do
  positive = Erlang.orddict.fetch(:do, options)
  negative = Erlang.orddict.fetch(:else, options)
  quote(if(unquote(clause), do: unquote(negative), else: unquote(positive)))
end

defmacro &&: [left, right] do
  quote(
    case unquote(left) do
    match: false
      false
    match: nil
      nil
    match: _
      unquote(right)
    end
  )
end

defmacro ||: [left, right] do
  quote(
    case !!(__oror_var = unquote(left)) do
    match: true
      __oror_var
    else:
      unquote(right)
    end
  )
end