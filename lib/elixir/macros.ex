ns Elixir::Macros

defmacro if(condition, [{:do,do_clause}|tail]) do
  matches = prepend_to_block(condition, do_clause)

  else_clause  = Orddict.fetch(tail, :else, nil)
  elsif_clause = Orddict.fetch(tail, :elsif, [])

  all = [matches|List.wrap(elsif_clause)]
  build_if_clauses(List.reverse(all), else_clause)
end

defmacro unless(clause, options) do
  quote(if(!unquote(clause), unquote(options)))
end

defmacro &&(left, right) do
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

defmacro ||(left, right) do
  quote(
    case !(__oror_var = unquote(left)) do
    match: false
      __oror_var
    else:
      unquote(right)
    end
  )
end

# Optimize !! to avoid generating case twice.
defmacro !({:!,_,[expr]}) do
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
defmacro !(expr) do
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

# private

# Build if clauses by nesting them recursively.
# For instance, the following clause:
#
#     if foo do
#       1
#     elsif: bar
#       2
#     else:
#       3
#     end
#
# Becomes:
#
#     case !foo do
#     match: false
#       1
#     match: true
#       case !bar do
#       match: false
#         2
#       match: true
#         3
#       end
#     end
#
def build_if_clauses([h|t], acc) do
  { condition, clause } = extract_condition_clause(h)

  new_acc = quote(
    case !unquote(condition) do
    match: false
      unquote(clause)
    match: true
      unquote(acc)
    end
  )

  build_if_clauses(t, new_acc)
end

def build_if_clauses([], acc), do: acc

# Extract condition clauses from blocks. Whenever we do:
#
#     if foo do
#       1
#     elsif bar
#       2
#       3
#     end
#
# This is transformed as a macro call to:
#
#     if(foo, do: 1, elsif: (bar; 2; 3))
#
# And, as we know, Elixir transforms (bar; 2; 3) into a block:
#
#     if(foo, do: 1, elsif: { :block, 0, [bar, 2, 3] })
#
# Therefore, this method simply extract the first argument from
# the block which is the argument used as condition.
def extract_condition_clause({ :block, line, [h|t] }), do: { h, { :block, line, t } }
def extract_condition_clause(other), do: { other, nil }

# Append the given expression to the block given as second argument.
# In case the second argument is not a block, create one.
def prepend_to_block(expr, { :block, line, args }) do
  { :block, line, [expr|args] }
end

def prepend_to_block(expr, args) do
  { :block, 0, [expr, args] }
end