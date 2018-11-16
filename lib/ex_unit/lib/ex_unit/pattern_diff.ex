defmodule ExUnit.Pattern do
  alias ExUnit.Pattern.DiffContext

  defstruct [:binary, :val, :match_single?, :vars, :pins]

  @type t :: %__MODULE__{
          binary: String.t(),
          val: any(),
          vars: map,
          pins: [key: atom()],
          match_single?: boolean()
        }

  def new(lh_pattern, pins, unbound_vars, match_single? \\ true)
      when is_list(pins) and is_map(unbound_vars) do
    %__MODULE__{
      binary: Macro.to_string(lh_pattern),
      val: lh_pattern,
      pins: pins,
      vars: unbound_vars,
      match_single?: match_single?
    }
  end

  def get_var({var, meta, context}) when is_atom(var) and is_atom(context) do
    {var, meta[:counter] || context}
  end

  def format_diff(left, right) do
    ctx = DiffContext.new_context(:none, left)

    compare = ExUnit.PatternDiff.compare(left, right)

    r_value =
      compare
      |> ExUnit.Pattern.FormatValue.format(ctx)
      |> Enum.reject(&(&1 == ""))

    l_value =
      compare
      |> ExUnit.Pattern.FormatPattern.format(ctx)
      |> ExUnit.Pattern.FormatPattern.commaize()
      |> List.flatten()

    {l_value, inspect(right)}
  end
end

defmodule ExUnit.ContainerDiff do
  defstruct [:type, :items]

  @type t :: %__MODULE__{
          type: :list | :map | :tuple | :struct | :when,
          items: [t | ExUnit.KeyDiff | ExUnit.PatternDiff.t()]
        }
end

defmodule ExUnit.WhenDiff do
  defstruct [:op, :bindings, :result]

  @type t :: %__MODULE__{
          op: atom(),
          bindings: Keyword.t(),
          result: :eq | :neq
        }
end

defmodule ExUnit.PinDiff do
  defstruct [:pin, :diff, :diff_result]
end

defmodule ExUnit.PatternDiff do
  alias ExUnit.{ContainerDiff, PinDiff, WhenDiff}

  defstruct [:type, :lh, :rh, :diff_result]

  @type lhs :: %{
          type: atom(),
          ast: any()
        }

  @type t :: %__MODULE__{
          type: :value | :key | :different | :map | :struct | :list | :tuple,
          lh: any(),
          rh: any(),
          diff_result: :eq | :neq
        }

  @no_value :ex_unit_no_meaningful_value

  # {:^, _, var}
  # {:|, _, [l, r]}
  # {:when, _, [l, r]}
  # {:=, _, [l, r]}
  # {:%{}, _, members}
  # {:{}, _, members}
  # {:_, _, _}
  # {var, _, context}
  # {key, val}
  # list
  # integer
  # float
  # string
  # atom

  def compare(pattern, r) do
    l = %{ast: pattern.val}
    {ret, _} = compare(l, r, {pattern.vars, pattern.pins})

    ret
  end

  def compare(%{ast: lh_list} = pattern, rh_list, env)
      when is_list(lh_list) and is_list(rh_list) do
    {items, env} = compare_list(pattern, rh_list, env)
    {%ContainerDiff{type: :list, items: items}, env}
  end

  def compare(%{ast: {:{}, _, _}} = pattern, rh_tuple, env) when is_tuple(rh_tuple) do
    {items, env} = compare_tuple(pattern, rh_tuple, env)
    {%ContainerDiff{type: :tuple, items: items}, env}
  end

  def compare(%{ast: {_, _}} = pattern, rh_tuple, env) when is_tuple(rh_tuple) do
    {items, env} = compare_tuple(pattern, rh_tuple, env)
    {%ContainerDiff{type: :tuple, items: items}, env}
  end

  def compare(%{ast: {:%{}, _, _}} = pattern, rh_map, env) when is_map(rh_map) do
    {items, env} = compare_map(pattern, rh_map, env)
    {%ContainerDiff{type: :map, items: items}, env}
  end

  def compare(%{ast: {:^, _, [{pin, _, _}]}} = pattern, rh_value, {_vars, pins} = env) do
    case Keyword.get(pins, pin) do
      ^rh_value ->
        {
          %__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :eq
          },
          env
        }

      other_value ->
        val = Macro.escape(other_value)
        {diff, _} = compare(%{ast: val}, rh_value, env)

        {
          %PinDiff{
            pin: pin,
            diff_result: :neq,
            diff: diff
          },
          env
        }
    end
  end

  def compare(%{ast: {:when, _, [ast, clause]}}, rh_value, env) do
    ast = %{ast: ast}
    {ast_result, env} = compare(ast, rh_value, env)
    when_result = evaluate_when(clause, env)
    {%ContainerDiff{type: :when, items: [ast_result, when_result]}, env}
  end

  def compare(%{ast: {:=, _, [l, r]}}, rh_value, env) do
    # Assignment in a pattern, identify which is the binding and which is the source data
    val =
      case l do
        {_, _, nil} -> r
        _ -> l
      end

    compare(%{ast: val}, rh_value, env)
  end

  def compare(%{ast: {:_, _, _}} = pattern, rh_value, env) do
    {
      %__MODULE__{
        type: :value,
        lh: pattern,
        rh: rh_value,
        diff_result: :eq
      },
      env
    }
  end

  def compare(%{ast: {var_name, _, var_ctx} = var} = pattern, rh_value, {vars, pins} = env)
      when is_atom(var_name) and is_atom(var_ctx) do
    var = get_var(var)

    case Map.get(vars, var, :ex_unit_unbound_var) do
      :ex_unit_unbound_var ->
        new_vars = Map.put(vars, var, rh_value)

        {
          %__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :eq
          },
          {new_vars, pins}
        }

      ^rh_value ->
        {
          %__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :eq
          },
          env
        }

      _other_value ->
        {
          %__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :neq
          },
          env
        }
    end
  end

  def compare(%{ast: lh_value} = pattern, rh_value, env)
      when is_integer(lh_value) and is_integer(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def compare(%{ast: lh_value} = pattern, rh_value, env)
      when is_float(lh_value) and is_float(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def compare(%{ast: lh_value} = pattern, rh_value, env)
      when is_binary(lh_value) and is_binary(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def compare(%{ast: lh_value} = pattern, rh_value, env)
      when is_atom(lh_value) and is_atom(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def compare(%{ast: _lh_value} = pattern, rh_value, env) do
    {
      %__MODULE__{
        type: :different,
        lh: pattern,
        rh: rh_value,
        diff_result: :neq
      },
      env
    }
  end

  def compare(@no_value, rh_value, env) do
    {
      %__MODULE__{
        type: :different,
        lh: @no_value,
        rh: rh_value,
        diff_result: :neq
      },
      env
    }
  end

  defp compare_list(%{ast: [{:|, _, [head, tail]} | _rest]}, [rh_head | rh_tail], env) do
    head_pattern = %{ast: head, type: :cons_l}
    tail_pattern = %{ast: tail, type: :cons_r}
    {h, env} = compare(head_pattern, rh_head, env)
    {t, env} = compare(tail_pattern, rh_tail, env)
    {[h, t], env}
  end

  defp compare_list(pattern, rh, env) do
    patterns = Enum.map(pattern.ast, &%{ast: &1})
    compare_list_items(patterns, rh, env)
  end

  defp compare_list_items([lh_h | lh_rest], [rh_h | rh_rest], env) do
    {h, env} = compare(lh_h, rh_h, env)
    {t, env} = compare_list_items(lh_rest, rh_rest, env)
    {[h | t], env}
  end

  defp compare_list_items([], [], env), do: {[], env}

  defp compare_list_items([lh_h | lh_rest], [], env) do
    {h, env} = compare(lh_h, @no_value, env)
    {t, env} = compare_list_items(lh_rest, [], env)
    {[h | t], env}
  end

  defp compare_list_items([], [rh_h | rh_rest], env) do
    {h, env} = compare(@no_value, rh_h, env)
    {t, env} = compare_list_items([], rh_rest, env)
    {[h | t], env}
  end

  defp compare_tuple(%{ast: {_, _}} = pattern, rh_tuple, env) do
    patterns =
      pattern.ast
      |> Tuple.to_list()
      |> Enum.map(&%{ast: &1})

    compare_list_items(patterns, Tuple.to_list(rh_tuple), env)
  end

  defp compare_tuple(%{ast: {:{}, _, members}}, rh_tuple, env) do
    patterns = Enum.map(members, &%{ast: &1})
    compare_list_items(patterns, Tuple.to_list(rh_tuple), env)
  end

  defp compare_map(%{ast: {:%{}, _, members}} = pattern, rh_map, env) do
    compare_map_items(pattern, members, rh_map, env)
  end

  defp compare_map_items(pattern, [lh_h | lh_t], rh_map, env) do
    {key, lh_value} = lh_h
    map_key = translate_key(key, env)

    {rh_value, rh_map} =
      case Map.pop(rh_map, map_key, @no_value) do
        {@no_value, rh_map} ->
          {@no_value, rh_map}

        {val, map} ->
          {{map_key, val}, map}
      end

    {h, env} = compare(%{ast: {key, lh_value}}, rh_value, env)
    {t, env} = compare_map_items(pattern, lh_t, rh_map, env)
    {[h | t], env}
  end

  defp compare_map_items(_pattern, [], rh_map, env) when rh_map == %{} do
    {[], env}
  end

  defp compare_map_items(pattern, [], rh_map, env) do
    lh = @no_value

    rh_key =
      rh_map
      |> Map.keys()
      |> List.first()

    {rh_value, rh_map} = Map.pop(rh_map, rh_key)
    {h, env} = compare(lh, {rh_key, rh_value}, env)
    {t, env} = compare_map_items(pattern, [], rh_map, env)
    {[h | t], env}
  end

  defp like_value_compare(%{ast: lh_value} = pattern, rh_value, env) do
    result = if lh_value == rh_value, do: :eq, else: :neq

    {
      %__MODULE__{
        type: :value,
        lh: pattern,
        rh: rh_value,
        diff_result: result
      },
      env
    }
  end

  defp evaluate_when({:and, _, [left, right]}, env) do
    l = evaluate_when(left, env)
    r = evaluate_when(right, env)
    result = if l.result == :eq && r.result == :eq, do: :eq, else: :neq
    %WhenDiff{op: :and, bindings: [l, r], result: result}
  end

  defp evaluate_when({:or, _, [left, right]}, env) do
    l = evaluate_when(left, env)
    r = evaluate_when(right, env)
    result = if l.result == :eq || r.result == :eq, do: :eq, else: :neq
    %WhenDiff{op: :or, bindings: [l, r], result: result}
  end

  defp evaluate_when({{:., _env, [:erlang, :andalso]}, env, when_vars}, {vars, pins}) do
    evaluate_when({:and, env, when_vars}, {vars, pins})
  end

  defp evaluate_when({{:., _env, [:erlang, :orelse]}, env, when_vars}, {vars, pins}) do
    evaluate_when({:or, env, when_vars}, {vars, pins})
  end

  defp evaluate_when({atom, _, when_vars}, {vars, _pins}) do
    v =
      when_vars
      |> Enum.map(fn var -> Map.get(vars, get_var(var)) end)

    bindings =
      when_vars
      |> Enum.map(fn var -> {get_var(var), Map.get(vars, get_var(var))} end)
      |> Map.new()

    result = if :erlang.apply(Kernel, atom, v), do: :eq, else: :neq

    %WhenDiff{op: atom, bindings: bindings, result: result}
  end

  defp translate_key({:^, _, [{pin, _, _}]}, {_vars, pins}) do
    pins[pin]
  end

  defp translate_key(rest, _) do
    rest
  end

  defp get_var({var, meta, context}) when is_atom(var) and is_atom(context) do
    {var, meta[:counter] || context}
  end
end
