ns Elixir::Macros

# Provides an integer division macro according to Erlang semantics.
# Raises an error if one of the arguments is not an integer.
# Can be used in guard tests.
#
# == Examples
#
#     5 div 2 #=> 2
#
defmacro div(left, right), do:
  quote(erlang_op :div, unquote(left), unquote(right))

# Provides an 'private' macro for restrict visibility of functions
#
# == Examples
#
#     ns Foo   # definition of Foo namespace
#
#     private  # make it function private
#     def secret do
#       :secret
#     end
#
#     endns
#     Foo.secret #=> it will raise 'undef' error
#
defmacro private do
  quote(Erlang.elixir_def_method.set_visibility(__NAMESPACE__, :private))
end

# Provides an integer remainder macro according to Erlang semantics.
# Raises an error if one of the arguments is not an integer.
# Can be used in guard tests.
#
# == Examples
#
#     5 rem 2 #=> 1
#
defmacro rem(left, right), do:
  quote(erlang_op :rem, unquote(left), unquote(right))

# Provides an `if` macro. The macro expects the first argument to
# be a condition and the rest are key-value arguments.
#
# == One-liner examples
#
#     if(foo, do: bar)
#
# In the example above, bar will be returned if foo evalutes to
# true (i.e. it is not false nor nil). Otherwise, nil will be returned.
#
# An else option can be given to specify the opposite:
#
#     if(foo, do: bar, else: bar)
#
# == Key-value blocks examples
#
# When several expressions must be passed to if, the most appropriate
# form is thorugh key-value blocks. The first example above would then
# be translated to:
#
#     if foo do
#       bar
#     end
#
# Notice that do/end becomes delimiters. The value given between
# do/end becomes the expression given to as `do:`. The second example
# would then translate do:
#
#     if foo do
#       bar
#     else:
#       baz
#     end
#
# Notice that extra keys follows the regular `else:` form. You can also
# add extra `elsif:` clauses:
#
#
#     if foo do
#       bar
#     elsif: some_condition
#       bar + baz
#     else:
#       baz
#     end
#
defmacro if(condition, [{:do,do_clause}|tail]) do
  matches = prepend_to_block(condition, do_clause)

  else_clause  = Orddict.fetch(tail, :else, nil)
  elsif_clause = Orddict.fetch(tail, :elsif, [])

  all = [matches|List.wrap(elsif_clause)]
  build_if_clauses(List.reverse(all), else_clause)
end

# Provide a unless macro that executes the expression
# unless a value evalutes to true. Check `if` for examples
# and documentation.
defmacro unless(clause, options) do
  quote(if(!unquote(clause), unquote(options)))
end

# Provide a short-circuit operator that executes the second
# expression only if the first one evalutes to true (i.e. it is
# not nil nor false). Returns the first expression otherwise.
#
# == Examples
#
#     true && true         #=> true
#     nil && true          #=> nil
#     true && 1            #=> 1
#     false && error(:bad) #=> false
#
# Notice that, differently from Erlang `and` and `andalso` operators,
# this operator accepts any expression as arguments, not only booleans.
# Unfortunately cannot be used in macros.
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

# Provide a short-circuit operator that executes the second
# expression only if the first one does not evalute to true (i.e. it
# is not nil nor false). Returns the first expression otherwise.
#
# == Examples
#
#     false || false       #=> false
#     nil || true          #=> true
#     false || 1           #=> 1
#     true || error(:bad)  #=> true
#
# Notice that, differently from Erlang `or` and `orelse` operators,
# this operator accepts any expression as arguments, not only booleans.
# Unfortunately cannot be used in macros.
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
# :nodoc:
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

# Implements the unary operator ! as a macro. It receives any
# argument and returns true if it is false or nil. Returns false
# otherwise.
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