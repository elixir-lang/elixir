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

# Optimize !! to avoid generating case twice.
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

# Implements the unary operator ! as a macro.
# It receives any argument and returns true if
# it is false or nil.
#
# == Examples
#
#   !1        #=> false
#   ![1,2,3]  #=> false
#   !false    #=> true
#   !nil      #=> true
#
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