ns Elixir::Macros

defmacro unless: [clause, options] do
  quote(if(!unquote(clause), unquote(options)))
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

defmacro !: [{:!,_,[expr]}] do
  quote(
    case unquote(expr) do
    match: false
      false
    match: nil
      false
    else:
      true
    end
  )
end

defmacro !: [expr] do
  quote(
    case unquote(expr) do
    match: false
      true
    match: nil
      true
    else:
      false
    end
  )
end