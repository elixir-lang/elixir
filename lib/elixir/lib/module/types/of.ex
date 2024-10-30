defmodule Module.Types.Of do
  # Typing functionality shared between Expr and Pattern.
  # Generic AST and Enum helpers go to Module.Types.Helpers.
  @moduledoc false

  alias Module.ParallelChecker
  import Module.Types.{Helpers, Descr}

  @prefix quote(do: ...)
  @suffix quote(do: ...)

  @integer_or_float union(integer(), float())
  @integer_or_binary union(integer(), binary())
  @integer integer()
  @float float()
  @binary binary()

  ## Variables

  @doc """
  Fetches the type of a defined variable.
  """
  def var({_name, meta, _context}, context) do
    version = Keyword.fetch!(meta, :version)
    %{vars: %{^version => %{type: type}}} = context
    type
  end

  @doc """
  Refines the type of a variable.
  """
  def refine_var(var, type, expr, formatter \\ :default, stack, context) do
    {var_name, meta, var_context} = var
    version = Keyword.fetch!(meta, :version)

    case context.vars do
      %{^version => %{type: old_type, off_traces: off_traces} = data} ->
        new_type = intersection(type, old_type)

        data = %{
          data
          | type: new_type,
            off_traces: new_trace(expr, type, formatter, stack, off_traces)
        }

        context = put_in(context.vars[version], data)

        # We need to return error otherwise it leads to cascading errors
        if empty?(new_type) do
          {:error, error_type(),
           error({:refine_var, old_type, type, var, context}, meta, stack, context)}
        else
          {:ok, new_type, context}
        end

      %{} ->
        data = %{
          type: type,
          name: var_name,
          context: var_context,
          off_traces: new_trace(expr, type, formatter, stack, [])
        }

        context = put_in(context.vars[version], data)
        {:ok, type, context}
    end
  end

  defp new_trace(nil, _type, _formatter, _stack, traces),
    do: traces

  defp new_trace(expr, type, formatter, stack, traces),
    do: [{expr, stack.file, type, formatter} | traces]

  ## Map/structs

  @doc """
  Handles fetching a map key.
  """
  def map_fetch(expr, type, field, stack, context) when is_atom(field) do
    case map_fetch(type, field) do
      {_optional?, value_type} ->
        {value_type, context}

      reason ->
        {error_type(), error({reason, expr, type, field, context}, elem(expr, 1), stack, context)}
    end
  end

  @doc """
  Builds a closed map.
  """
  def closed_map(pairs, extra \\ [], stack, context, of_fun) do
    {closed?, single, multiple, context} =
      Enum.reduce(pairs, {true, extra, [], context}, fn
        {key, value}, {closed?, single, multiple, context} ->
          {keys, context} = of_finite_key_type(key, stack, context, of_fun)
          {value_type, context} = of_fun.(value, stack, context)

          case keys do
            :none ->
              {false, single, multiple, context}

            [key] when multiple == [] ->
              {closed?, [{key, value_type} | single], multiple, context}

            keys ->
              {closed?, single, [{keys, value_type} | multiple], context}
          end
      end)

    map =
      case Enum.reverse(multiple) do
        [] ->
          pairs = Enum.reverse(single)
          if closed?, do: closed_map(pairs), else: open_map(pairs)

        [{keys, type} | tail] ->
          for key <- keys, t <- cartesian_map(tail) do
            pairs = Enum.reverse(single, [{key, type} | t])
            if closed?, do: closed_map(pairs), else: open_map(pairs)
          end
          |> Enum.reduce(&union/2)
      end

    {map, context}
  end

  defp of_finite_key_type(key, _stack, context, _of_fun) when is_atom(key) do
    {[key], context}
  end

  defp of_finite_key_type(key, stack, context, of_fun) do
    {key_type, context} = of_fun.(key, stack, context)

    case atom_fetch(key_type) do
      {:finite, list} -> {list, context}
      _ -> {:none, context}
    end
  end

  defp cartesian_map(lists) do
    case lists do
      [] ->
        [[]]

      [{keys, type} | tail] ->
        for key <- keys, t <- cartesian_map(tail), do: [{key, type} | t]
    end
  end

  @doc """
  Handles structs creation.
  """
  def struct({:%, meta, _}, struct, args, default_handling, stack, context, of_fun)
      when is_atom(struct) do
    # The compiler has already checked the keys are atoms and which ones are required.
    {args_types, context} =
      Enum.map_reduce(args, context, fn {key, value}, context when is_atom(key) ->
        {type, context} = of_fun.(value, stack, context)
        {{key, type}, context}
      end)

    struct(struct, args_types, default_handling, meta, stack, context)
  end

  @doc """
  Struct handling assuming the args have already been converted.
  """
  # TODO: Allow structs fields to be defined. If the fields are defined,
  # then the struct is no longer dynamic. And we need to validate args
  # against the struct types.
  # TODO: Use the struct default values to define the default types.
  def struct(struct, args_types, default_handling, meta, stack, context) do
    {info, context} = struct_info(struct, meta, stack, context)
    term = term()
    defaults = for %{field: field} <- info, do: {field, term}

    pairs =
      case default_handling do
        :merge_defaults -> [{:__struct__, atom([struct])} | defaults] ++ args_types
        :skip_defaults -> [{:__struct__, atom([struct])} | args_types]
        :only_defaults -> [{:__struct__, atom([struct])} | defaults]
      end

    {dynamic(closed_map(pairs)), context}
  end

  @doc """
  Returns `__info__(:struct)` information about a struct.
  """
  def struct_info(struct, meta, stack, context) do
    {_, context} = remote(struct, :__struct__, 0, meta, stack, context)

    info =
      struct.__info__(:struct) ||
        raise "expected #{inspect(struct)} to return struct metadata, but got none"

    {info, context}
  end

  ## Binary

  @doc """
  Handles binaries.

  In the stack, we add nodes such as <<expr>>, <<..., expr>>, etc,
  based on the position of the expression within the binary.
  """
  def binary([], _kind, _stack, context) do
    context
  end

  def binary([head], kind, stack, context) do
    binary_segment(head, kind, [head], stack, context)
  end

  def binary([head | tail], kind, stack, context) do
    context = binary_segment(head, kind, [head, @suffix], stack, context)
    binary_many(tail, kind, stack, context)
  end

  defp binary_many([last], kind, stack, context) do
    binary_segment(last, kind, [@prefix, last], stack, context)
  end

  defp binary_many([head | tail], kind, stack, context) do
    context = binary_segment(head, kind, [@prefix, head, @suffix], stack, context)
    binary_many(tail, kind, stack, context)
  end

  # If the segment is a literal, the compiler has already checked its validity,
  # so we just skip it.
  defp binary_segment({:"::", _meta, [left, _right]}, _kind, _args, _stack, context)
       when is_binary(left) or is_number(left) do
    context
  end

  defp binary_segment({:"::", meta, [left, right]}, kind, args, stack, context) do
    type = specifier_type(kind, right)
    expr = {:<<>>, meta, args}

    {_type, context} =
      case kind do
        :match ->
          Module.Types.Pattern.of_match_var(left, type, expr, stack, context)

        :guard ->
          Module.Types.Pattern.of_guard(left, type, expr, stack, context)

        :expr ->
          {actual, context} = Module.Types.Expr.of_expr(left, stack, context)
          intersect(actual, type, expr, stack, context)
      end

    specifier_size(kind, right, stack, context)
  end

  defp specifier_type(kind, {:-, _, [left, _right]}), do: specifier_type(kind, left)
  defp specifier_type(:match, {:utf8, _, _}), do: @integer
  defp specifier_type(:match, {:utf16, _, _}), do: @integer
  defp specifier_type(:match, {:utf32, _, _}), do: @integer
  defp specifier_type(:match, {:float, _, _}), do: @float
  defp specifier_type(_kind, {:float, _, _}), do: @integer_or_float
  defp specifier_type(_kind, {:utf8, _, _}), do: @integer_or_binary
  defp specifier_type(_kind, {:utf16, _, _}), do: @integer_or_binary
  defp specifier_type(_kind, {:utf32, _, _}), do: @integer_or_binary
  defp specifier_type(_kind, {:integer, _, _}), do: @integer
  defp specifier_type(_kind, {:bits, _, _}), do: @binary
  defp specifier_type(_kind, {:bitstring, _, _}), do: @binary
  defp specifier_type(_kind, {:bytes, _, _}), do: @binary
  defp specifier_type(_kind, {:binary, _, _}), do: @binary
  defp specifier_type(_kind, _specifier), do: @integer

  defp specifier_size(kind, {:-, _, [left, right]}, stack, context) do
    specifier_size(kind, right, stack, specifier_size(kind, left, stack, context))
  end

  defp specifier_size(:expr, {:size, _, [arg]} = expr, stack, context)
       when not is_integer(arg) do
    {actual, context} = Module.Types.Expr.of_expr(arg, stack, context)
    {_, context} = intersect(actual, integer(), expr, stack, context)
    context
  end

  defp specifier_size(_pattern_or_guard, {:size, _, [arg]} = expr, stack, context)
       when not is_integer(arg) do
    {_type, context} = Module.Types.Pattern.of_guard(arg, integer(), expr, stack, context)
    context
  end

  defp specifier_size(_kind, _specifier, _stack, context) do
    context
  end

  ## Modules

  @doc """
  Returns the modules.

  The call information is used on report reporting.
  """
  def modules(type, fun, arity, hints \\ [], expr, meta, stack, context) do
    case atom_fetch(type) do
      {_, mods} ->
        {mods, context}

      :error ->
        warning = {:badmodule, expr, type, fun, arity, hints, context}
        {[], error(warning, meta, stack, context)}
    end
  end

  ## Remotes

  # Define strong arrows found in the standard library.
  # A strong arrow means that, if a type outside of its
  # domain is given, an error is raised. We are also
  # ensuring that domains for the same function have
  # no overlaps.

  mfargs = [atom(), atom(), list(term())]

  send_destination =
    pid()
    |> union(reference())
    |> union(port())
    |> union(atom())
    |> union(tuple([atom(), atom()]))

  basic_arith_2_args_clauses = [
    {[integer(), integer()], integer()},
    {[integer(), float()], float()},
    {[float(), integer()], float()},
    {[float(), float()], float()}
  ]

  is_clauses = [{[term()], boolean()}]

  for {mod, fun, clauses} <- [
        # :binary
        {:binary, :copy, [{[binary(), integer()], binary()}]},

        # :erlang
        {:erlang, :+, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :+, basic_arith_2_args_clauses},
        {:erlang, :-, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :-, basic_arith_2_args_clauses},
        {:erlang, :*, basic_arith_2_args_clauses},
        {:erlang, :/, [{[union(integer(), float()), union(integer(), float())], float()}]},
        {:erlang, :"/=", [{[term(), term()], boolean()}]},
        {:erlang, :"=/=", [{[term(), term()], boolean()}]},
        {:erlang, :<, [{[term(), term()], boolean()}]},
        {:erlang, :"=<", [{[term(), term()], boolean()}]},
        {:erlang, :==, [{[term(), term()], boolean()}]},
        {:erlang, :"=:=", [{[term(), term()], boolean()}]},
        {:erlang, :>, [{[term(), term()], boolean()}]},
        {:erlang, :>=, [{[term(), term()], boolean()}]},
        {:erlang, :abs, [{[integer()], integer()}, {[float()], float()}]},
        {:erlang, :atom_to_binary, [{[atom()], binary()}]},
        {:erlang, :atom_to_list, [{[atom()], list(integer())}]},
        {:erlang, :band, [{[integer(), integer()], integer()}]},
        {:erlang, :binary_part, [{[binary(), integer(), integer()], binary()}]},
        {:erlang, :binary_to_atom, [{[binary()], atom()}]},
        {:erlang, :binary_to_existing_atom, [{[binary()], atom()}]},
        {:erlang, :binary_to_integer, [{[binary()], integer()}]},
        {:erlang, :binary_to_integer, [{[binary(), integer()], integer()}]},
        {:erlang, :binary_to_float, [{[binary()], float()}]},
        {:erlang, :bit_size, [{[binary()], integer()}]},
        {:erlang, :bnot, [{[integer()], integer()}]},
        {:erlang, :bor, [{[integer(), integer()], integer()}]},
        {:erlang, :bsl, [{[integer(), integer()], integer()}]},
        {:erlang, :bsr, [{[integer(), integer()], integer()}]},
        {:erlang, :bxor, [{[integer(), integer()], integer()}]},
        {:erlang, :byte_size, [{[binary()], integer()}]},
        {:erlang, :ceil, [{[union(integer(), float())], integer()}]},
        {:erlang, :div, [{[integer(), integer()], integer()}]},
        {:erlang, :floor, [{[union(integer(), float())], integer()}]},
        {:erlang, :function_exported, [{[atom(), atom(), integer()], boolean()}]},
        {:erlang, :integer_to_binary, [{[integer()], binary()}]},
        {:erlang, :integer_to_binary, [{[integer(), integer()], binary()}]},
        {:erlang, :integer_to_list, [{[integer()], non_empty_list(integer())}]},
        {:erlang, :integer_to_list, [{[integer(), integer()], non_empty_list(integer())}]},
        {:erlang, :is_atom, is_clauses},
        {:erlang, :is_binary, is_clauses},
        {:erlang, :is_bitstring, is_clauses},
        {:erlang, :is_boolean, is_clauses},
        {:erlang, :is_float, is_clauses},
        {:erlang, :is_function, is_clauses},
        {:erlang, :is_function, [{[term(), integer()], boolean()}]},
        {:erlang, :is_integer, is_clauses},
        {:erlang, :is_list, is_clauses},
        {:erlang, :is_map, is_clauses},
        {:erlang, :is_map_key, [{[term(), open_map()], boolean()}]},
        {:erlang, :is_number, is_clauses},
        {:erlang, :is_pid, is_clauses},
        {:erlang, :is_port, is_clauses},
        {:erlang, :is_reference, is_clauses},
        {:erlang, :is_tuple, is_clauses},
        {:erlang, :length, [{[list(term())], integer()}]},
        {:erlang, :list_to_atom, [{[list(integer())], atom()}]},
        {:erlang, :list_to_existing_atom, [{[list(integer())], atom()}]},
        {:erlang, :list_to_float, [{[non_empty_list(integer())], float()}]},
        {:erlang, :list_to_integer, [{[non_empty_list(integer())], integer()}]},
        {:erlang, :list_to_integer, [{[non_empty_list(integer()), integer()], integer()}]},
        {:erlang, :list_to_tuple, [{[list(term())], dynamic(open_tuple([]))}]},
        {:erlang, :make_ref, [{[], reference()}]},
        {:erlang, :map_size, [{[open_map()], integer()}]},
        {:erlang, :node, [{[], atom()}]},
        {:erlang, :node, [{[pid() |> union(reference()) |> union(port())], atom()}]},
        {:erlang, :not, [{[atom([false])], atom([true])}, {[atom([true])], atom([false])}]},
        {:erlang, :rem, [{[integer(), integer()], integer()}]},
        {:erlang, :round, [{[union(integer(), float())], integer()}]},
        {:erlang, :self, [{[], pid()}]},
        {:erlang, :spawn, [{[fun()], pid()}]},
        {:erlang, :spawn, [{mfargs, pid()}]},
        {:erlang, :spawn_link, [{[fun()], pid()}]},
        {:erlang, :spawn_link, [{mfargs, pid()}]},
        {:erlang, :spawn_monitor, [{[fun()], tuple([reference(), pid()])}]},
        {:erlang, :spawn_monitor, [{mfargs, tuple([reference(), pid()])}]},
        {:erlang, :tuple_size, [{[open_tuple([])], integer()}]},
        {:erlang, :trunc, [{[union(integer(), float())], integer()}]},

        # TODO: Replace term()/dynamic() by parametric types
        {:erlang, :++, [{[list(term()), term()], dynamic(list(term(), term()))}]},
        {:erlang, :--, [{[list(term()), list(term())], dynamic(list(term()))}]},
        {:erlang, :delete_element, [{[integer(), open_tuple([])], dynamic(open_tuple([]))}]},
        {:erlang, :hd, [{[non_empty_list(term(), term())], dynamic()}]},
        {:erlang, :element, [{[integer(), open_tuple([])], dynamic()}]},
        {:erlang, :insert_element,
         [{[integer(), open_tuple([]), term()], dynamic(open_tuple([]))}]},
        {:erlang, :max, [{[term(), term()], dynamic()}]},
        {:erlang, :min, [{[term(), term()], dynamic()}]},
        {:erlang, :send, [{[send_destination, term()], dynamic()}]},
        {:erlang, :setelement, [{[integer(), open_tuple([]), term()], dynamic(open_tuple([]))}]},
        {:erlang, :tl, [{[non_empty_list(term(), term())], dynamic()}]},
        {:erlang, :tuple_to_list, [{[open_tuple([])], dynamic(list(term()))}]}
      ] do
    [arity] = Enum.map(clauses, fn {args, _return} -> length(args) end) |> Enum.uniq()
    true = Code.ensure_loaded?(mod) and function_exported?(mod, fun, arity)

    domain_clauses =
      case clauses do
        [_] ->
          {:strong, nil, clauses}

        _ ->
          domain =
            clauses
            |> Enum.map(fn {args, _} -> args end)
            |> Enum.zip_with(fn types -> Enum.reduce(types, &union/2) end)

          {:strong, domain, clauses}
      end

    defp remote(unquote(mod), unquote(fun), unquote(arity)),
      do: unquote(Macro.escape(domain_clauses))
  end

  defp remote(_mod, _fun, _arity), do: :none

  @doc """
  Checks a module is a valid remote.

  It returns either a tuple with the remote information and the context.
  The remote information may be one of:

    * `:none` - no typing information found.

    * `{:infer, clauses}` - clauses from inferences. You must check all
      all clauses and return the union between them. They are dynamic
      and they can only be converted into arrows by computing the union
      of all arguments.

    * `{:strong, domain or nil, clauses}` - clauses from signatures. So far
      these are strong arrows with non-overlapping domains

  """
  def remote(module, fun, arity, meta, stack, context) when is_atom(module) do
    if Keyword.get(meta, :runtime_module, false) do
      {:none, context}
    else
      case remote(module, fun, arity) do
        :none -> {:none, check_export(module, fun, arity, meta, stack, context)}
        clauses -> {clauses, context}
      end
    end
  end

  # TODO: Fix ordering of tuple operations

  def apply(:erlang, :element, [_, tuple], {_, meta, [index, _]} = expr, stack, context)
      when is_integer(index) do
    case tuple_fetch(tuple, index - 1) do
      {_optional?, value_type} ->
        {value_type, context}

      :badtuple ->
        {error_type(), to_badapply_error(expr, [integer(), tuple], stack, context)}

      reason ->
        {error_type(), error({reason, expr, tuple, index - 1, context}, meta, stack, context)}
    end
  end

  def apply(
        :erlang,
        :insert_element,
        [_, tuple, value],
        {_, meta, [index, _, _]} = expr,
        stack,
        context
      )
      when is_integer(index) do
    case tuple_insert_at(tuple, index - 1, value) do
      value_type when is_descr(value_type) ->
        {value_type, context}

      :badtuple ->
        {error_type(), to_badapply_error(expr, [integer(), tuple, value], stack, context)}

      reason ->
        {error_type(), error({reason, expr, tuple, index - 2, context}, meta, stack, context)}
    end
  end

  def apply(:erlang, :delete_element, [_, tuple], {_, meta, [index, _]} = expr, stack, context)
      when is_integer(index) do
    case tuple_delete_at(tuple, index - 1) do
      value_type when is_descr(value_type) ->
        {value_type, context}

      :badtuple ->
        {error_type(), to_badapply_error(expr, [integer(), tuple], stack, context)}

      reason ->
        {error_type(), error({reason, expr, tuple, index - 1, context}, meta, stack, context)}
    end
  end

  def apply(:erlang, :make_tuple, [_, elem], {_, _meta, [size, _]}, _stack, context)
      when is_integer(size) and size >= 0 do
    {tuple(List.duplicate(elem, size)), context}
  end

  def apply(:erlang, :hd, [list], expr, stack, context) do
    case list_hd(list) do
      {_, value_type} ->
        {value_type, context}

      :badnonemptylist ->
        {error_type(), to_badapply_error(expr, [list], stack, context)}
    end
  end

  def apply(:erlang, :tl, [list], expr, stack, context) do
    case list_tl(list) do
      {_, value_type} ->
        {value_type, context}

      :badnonemptylist ->
        {error_type(), to_badapply_error(expr, [list], stack, context)}
    end
  end

  def apply(:erlang, name, [left, right] = args_types, expr, stack, context)
      when name in [:>=, :"=<", :>, :<, :min, :max] do
    context =
      cond do
        match?({false, _}, map_fetch(left, :__struct__)) or
            match?({false, _}, map_fetch(right, :__struct__)) ->
          warning = {:struct_comparison, expr, context}
          warn(__MODULE__, warning, elem(expr, 1), stack, context)

        number_type?(left) and number_type?(right) ->
          context

        disjoint?(left, right) ->
          warning = {:mismatched_comparison, expr, context}
          warn(__MODULE__, warning, elem(expr, 1), stack, context)

        true ->
          context
      end

    if name in [:min, :max] do
      {union(left, right), context}
    else
      {remote_return(boolean(), args_types, stack), context}
    end
  end

  def apply(:erlang, name, [left, right] = args_types, expr, stack, context)
      when name in [:==, :"/=", :"=:=", :"=/="] do
    context =
      cond do
        name in [:==, :"/="] and number_type?(left) and number_type?(right) ->
          context

        disjoint?(left, right) ->
          warning = {:mismatched_comparison, expr, context}
          warn(__MODULE__, warning, elem(expr, 1), stack, context)

        true ->
          context
      end

    {remote_return(boolean(), args_types, stack), context}
  end

  def apply(mod, name, args_types, expr, stack, context) do
    arity = length(args_types)

    case :elixir_rewrite.inline(mod, name, arity) do
      {mod, name} ->
        apply(mod, name, args_types, expr, stack, context)

      false ->
        {info, context} = remote(mod, name, arity, elem(expr, 1), stack, context)

        case apply_remote(info, args_types, stack) do
          {:ok, type} ->
            {type, context}

          {:error, domain, clauses} ->
            error = {:badapply, expr, args_types, domain, clauses, context}
            {error_type(), error(error, elem(expr, 1), stack, context)}
        end
    end
  end

  defp remote_return(type, args_types, stack) do
    cond do
      stack.mode == :static -> type
      Enum.any?(args_types, &gradual?/1) -> dynamic(type)
      true -> type
    end
  end

  defp apply_remote(:none, _args_types, _stack) do
    {:ok, dynamic()}
  end

  defp apply_remote({:strong, nil, [{expected, return}] = clauses}, args_types, stack) do
    # Optimize single clauses as the domain is the single clause args.
    case zip_compatible?(args_types, expected) do
      true -> {:ok, remote_return(return, args_types, stack)}
      false -> {:error, expected, clauses}
    end
  end

  defp apply_remote({:strong, domain, clauses}, args_types, stack) do
    # If the type is only gradual, the compatibility check is the same
    # as a non disjoint check. So we skip checking compatibility twice.
    with true <- zip_compatible_or_only_gradual?(args_types, domain),
         [_ | _] = returns <-
           for({expected, return} <- clauses, zip_not_disjoint?(args_types, expected), do: return) do
      {:ok, returns |> Enum.reduce(&union/2) |> remote_return(args_types, stack)}
    else
      _ -> {:error, domain, clauses}
    end
  end

  defp zip_compatible_or_only_gradual?([actual | actuals], [expected | expecteds]) do
    (only_gradual?(actual) or compatible?(actual, expected)) and
      zip_compatible_or_only_gradual?(actuals, expecteds)
  end

  defp zip_compatible_or_only_gradual?([], []), do: true

  defp zip_compatible?([actual | actuals], [expected | expecteds]) do
    compatible?(actual, expected) and zip_compatible?(actuals, expecteds)
  end

  defp zip_compatible?([], []), do: true

  defp zip_not_disjoint?([actual | actuals], [expected | expecteds]) do
    not disjoint?(actual, expected) and zip_not_disjoint?(actuals, expecteds)
  end

  defp zip_not_disjoint?([], []), do: true

  defp check_export(module, fun, arity, meta, stack, context) do
    case ParallelChecker.fetch_export(stack.cache, module, fun, arity) do
      {:ok, mode, :def, reason} ->
        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:ok, mode, :defmacro, reason} ->
        context =
          warn(__MODULE__, {:unrequired_module, module, fun, arity}, meta, stack, context)

        check_deprecated(mode, module, fun, arity, reason, meta, stack, context)

      {:error, :module} ->
        if warn_undefined?(module, fun, arity, stack) do
          warn(__MODULE__, {:undefined_module, module, fun, arity}, meta, stack, context)
        else
          context
        end

      {:error, :function} ->
        if warn_undefined?(module, fun, arity, stack) do
          exports = ParallelChecker.all_exports(stack.cache, module)
          payload = {:undefined_function, module, fun, arity, exports}
          warn(__MODULE__, payload, meta, stack, context)
        else
          context
        end
    end
  end

  defp check_deprecated(:elixir, module, fun, arity, reason, meta, stack, context) do
    if reason do
      warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)
    else
      context
    end
  end

  defp check_deprecated(:erlang, module, fun, arity, _reason, meta, stack, context) do
    case :otp_internal.obsolete(module, fun, arity) do
      {:deprecated, string} when is_list(string) ->
        reason = string |> List.to_string() |> :string.titlecase()
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      {:deprecated, string, removal} when is_list(string) and is_list(removal) ->
        reason = string |> List.to_string() |> :string.titlecase()
        reason = "It will be removed in #{removal}. #{reason}"
        warn(__MODULE__, {:deprecated, module, fun, arity, reason}, meta, stack, context)

      _ ->
        context
    end
  end

  # The protocol code dispatches to unknown modules, so we ignore them here.
  #
  #     try do
  #       SomeProtocol.Atom.__impl__
  #     rescue
  #       ...
  #     end
  #
  # But for protocols we don't want to traverse the protocol code anyway.
  # TODO: remove this clause once we no longer traverse the protocol code.
  defp warn_undefined?(_module, :__impl__, 1, _stack), do: false
  defp warn_undefined?(_module, :module_info, 0, _stack), do: false
  defp warn_undefined?(_module, :module_info, 1, _stack), do: false
  defp warn_undefined?(:erlang, :orelse, 2, _stack), do: false
  defp warn_undefined?(:erlang, :andalso, 2, _stack), do: false

  defp warn_undefined?(_, _, _, %{no_warn_undefined: :all}) do
    false
  end

  defp warn_undefined?(module, fun, arity, stack) do
    not Enum.any?(stack.no_warn_undefined, &(&1 == module or &1 == {module, fun, arity}))
  end

  ## Warning helpers

  @doc """
  Intersects two types and emit an incompatible error if empty.
  """
  def intersect(actual, expected, expr, stack, context) do
    type = intersection(actual, expected)

    if empty?(type) do
      {error_type(), incompatible_error(expr, expected, actual, stack, context)}
    else
      {type, context}
    end
  end

  @doc """
  Emits incompatible types warning for the given expression.

  This is a generic warning for when the expected/actual types
  themselves may come from several different circumstances.
  """
  def incompatible_error(expr, expected_type, actual_type, stack, context) do
    meta = get_meta(expr) || stack.meta
    hints = if meta[:inferred_bitstring_spec], do: [:inferred_bitstring_spec], else: []
    warning = {:incompatible, expr, expected_type, actual_type, hints, context}
    error(warning, meta, stack, context)
  end

  defp error(warning, meta, stack, context) do
    error(__MODULE__, warning, meta, stack, context)
  end

  ## Warning formatting

  def format_diagnostic({:refine_var, old_type, new_type, var, context}) do
    traces = collect_traces(var, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types assigned to #{format_var(var)}:

              #{to_quoted_string(old_type)} !~ #{to_quoted_string(new_type)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:incompatible, expr, expected_type, actual_type, hints, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types in expression:

              #{expr_to_string(expr) |> indent(4)}

          got type:

              #{to_quoted_string(actual_type) |> indent(4)}

          but expected type:

              #{to_quoted_string(expected_type) |> indent(4)}
          """,
          format_traces(traces),
          format_hints(hints)
        ])
    }
  end

  def format_diagnostic({:badmap, expr, type, key, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a map or struct when accessing .#{key} in expression:

              #{expr_to_string(expr) |> indent(4)}
          """,
          empty_if(dot_var?(expr), """

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """),
          format_traces(traces),
          format_hints([:dot])
        ])
    }
  end

  def format_diagnostic({:badkey, expr, type, key, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      span: expr |> get_meta() |> :elixir_env.calculate_span(key) |> Keyword.get(:span),
      message:
        IO.iodata_to_binary([
          """
          unknown key .#{key} in expression:

              #{expr_to_string(expr) |> indent(4)}
          """,
          empty_if(dot_var?(expr), """

          the given type does not have the given key:

              #{to_quoted_string(type) |> indent(4)}
          """),
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badindex, expr, type, index, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a tuple with at least #{pluralize(index + 1, "element", "elements")} in #{format_mfa(expr)}:

              #{expr_to_string(expr) |> indent(4)}

          the given type does not have the given index:

              #{to_quoted_string(type) |> indent(4)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:badmodule, expr, type, fun, arity, hints, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          expected a module (an atom) when invoking #{fun}/#{arity} in expression:

              #{expr_to_string(expr) |> indent(4)}
          """,
          empty_if(dot_var?(expr), """

          but got type:

              #{to_quoted_string(type) |> indent(4)}
          """),
          format_traces(traces),
          format_hints(hints)
        ])
    }
  end

  def format_diagnostic({:badapply, expr, args_types, domain, clauses, context}) do
    traces = collect_traces(expr, context)
    {{:., _, [mod, fun]}, _, args} = expr

    {mod, fun, args, converter} = :elixir_rewrite.erl_to_ex(mod, fun, args)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          incompatible types given to #{Exception.format_mfa(mod, fun, length(args))}:

              #{expr_to_string(expr) |> indent(4)}

          given types:

              #{args_to_quoted_string(args_types, domain, converter) |> indent(4)}

          but expected one of:
          #{clauses_args_to_quoted_string(clauses, converter)}
          """,
          format_traces(traces)
        ])
    }
  end

  def format_diagnostic({:mismatched_comparison, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison between distinct types found:

              #{expr_to_string(expr) |> indent(4)}
          """,
          format_traces(traces),
          """

          While Elixir can compare across all types, you are comparing \
          across types which are always disjoint, and the result is either \
          always true or always false
          """
        ])
    }
  end

  def format_diagnostic({:struct_comparison, expr, context}) do
    traces = collect_traces(expr, context)

    %{
      details: %{typing_traces: traces},
      message:
        IO.iodata_to_binary([
          """
          comparison with structs found:

              #{expr_to_string(expr) |> indent(4)}
          """,
          format_traces(traces),
          """

          Comparison operators (>, <, >=, <=, min, and max) perform structural \
          and not semantic comparison. Comparing with a struct won't give meaningful \
          results. Structs that can be compared typically define a compare/2 function \
          within their modules that can be used for semantic comparison.
          """
        ])
    }
  end

  def format_diagnostic({:undefined_module, module, fun, arity}) do
    top =
      if fun == :__struct__ and arity == 0 do
        "struct #{inspect(module)}"
      else
        Exception.format_mfa(module, fun, arity)
      end

    %{
      message:
        IO.iodata_to_binary([
          top,
          " is undefined (module ",
          inspect(module),
          " is not available or is yet to be defined)",
          UndefinedFunctionError.hint_for_missing_module(module, fun, arity)
        ]),
      group: true
    }
  end

  def format_diagnostic({:undefined_function, module, :__struct__, 0, _exports}) do
    %{
      message:
        "struct #{inspect(module)} is undefined (there is such module but it does not define a struct)",
      group: true
    }
  end

  def format_diagnostic({:undefined_function, module, fun, arity, exports}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is undefined or private",
          UndefinedFunctionError.hint_for_loaded_module(module, fun, arity, exports)
        ]),
      group: true
    }
  end

  def format_diagnostic({:deprecated, module, fun, arity, reason}) do
    %{
      message:
        IO.iodata_to_binary([
          Exception.format_mfa(module, fun, arity),
          " is deprecated. ",
          reason
        ]),
      group: true
    }
  end

  def format_diagnostic({:unrequired_module, module, fun, arity}) do
    %{
      message:
        IO.iodata_to_binary([
          "you must require ",
          inspect(module),
          " before invoking the macro ",
          Exception.format_mfa(module, fun, arity)
        ]),
      group: true
    }
  end

  defp pluralize(1, singular, _), do: "1 #{singular}"
  defp pluralize(i, _, plural), do: "#{i} #{plural}"

  defp dot_var?(expr) do
    match?({{:., _, [var, _fun]}, _, _args} when is_var(var), expr)
  end

  defp to_badapply_error({{:., _, [mod, fun]}, meta, _} = expr, args_types, stack, context) do
    {_type, domain, [{args, _} | _] = clauses} = remote(mod, fun, length(args_types))
    error({:badapply, expr, args_types, domain || args, clauses, context}, meta, stack, context)
  end

  defp empty_if(condition, content) do
    if condition, do: "", else: content
  end

  defp format_mfa({{:., _, [mod, fun]}, _, args}) do
    {mod, fun, args, _} = :elixir_rewrite.erl_to_ex(mod, fun, args)
    Exception.format_mfa(mod, fun, length(args))
  end

  ## Algebra helpers

  alias Inspect.Algebra, as: IA

  defp clauses_args_to_quoted_string([{args, _return}], converter) do
    "\n    " <> (clause_args_to_quoted_string(args, converter) |> indent(4))
  end

  defp clauses_args_to_quoted_string(clauses, converter) do
    clauses
    |> Enum.with_index(fn {args, _return}, index ->
      """

      ##{index + 1}
      #{clause_args_to_quoted_string(args, converter)}\
      """
      |> indent(4)
    end)
    |> Enum.join("\n")
  end

  defp clause_args_to_quoted_string(args, converter) do
    docs = Enum.map(args, &(&1 |> to_quoted() |> Code.Formatter.to_algebra()))
    args_docs_to_quoted_string(converter.(docs))
  end

  defp args_to_quoted_string(args_types, domain, converter) do
    ansi? = IO.ANSI.enabled?()

    docs =
      Enum.zip_with(args_types, domain, fn actual, expected ->
        doc = actual |> to_quoted() |> Code.Formatter.to_algebra()

        cond do
          compatible?(actual, expected) -> doc
          ansi? -> IA.concat(IA.color(doc, IO.ANSI.red()), IA.color(IA.empty(), IO.ANSI.reset()))
          true -> IA.concat(["-", doc, "-"])
        end
      end)

    args_docs_to_quoted_string(converter.(docs))
  end

  defp args_docs_to_quoted_string(docs) do
    doc = IA.fold(docs, fn doc, acc -> IA.glue(IA.concat(doc, ","), acc) end)

    wrapped_docs =
      case docs do
        [_] -> IA.concat("(", IA.concat(doc, ")"))
        _ -> IA.group(IA.glue(IA.nest(IA.glue("(", "", doc), 2), "", ")"))
      end

    wrapped_docs
    |> IA.format(98)
    |> IO.iodata_to_binary()
    |> case do
      "(\n" <> _ = multiple_lines -> multiple_lines
      single_line -> binary_slice(single_line, 1..-2//1)
    end
  end
end
