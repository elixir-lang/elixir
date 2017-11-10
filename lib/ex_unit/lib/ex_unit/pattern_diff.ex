defmodule ExUnit.ContainerDiff do
  defstruct [:type, :items]
  @type t :: %__MODULE__{
    type: :list | :map | :tuple | :struct,
    items: [t | ExUnit.KeyDiff | ExUnit.PatternDiff.t],
  }
end

defmodule ExUnit.PatternDiff do
  alias ExUnit.ContainerDiff
  alias ExUnit.Pattern

  defstruct [:type, :lh, :rh, :diff_result]
  @type t :: %__MODULE__{
    type: :value | :key | :different | :map | :struct | :list | :tuple,
    lh: any(),
    rh: any(),
    diff_result: :eq | :neq,
  }

  @no_value :ex_unit_no_meaningful_value

  # {:^, _, var}
  # {:|, _, [l, r]}
  # {:when, _, [l, r]}
  # {:%{}, _, members}
  # {:{}, _, members}
  # {var, _, _}
  # {key, val}
  # list
  # integer
  # float
  # string
  # atom

  # @type t :: %__MODULE__{
  #   val: any(),
  #   vars: [key: atom()],
  #   pins: [key: atom()],
  # }

  def cmp(l, r) do
    {ret, _} = cmp(l, r, {l.vars, l.pins})
    ret
  end

  def cmp(%{meta: :list} = pattern, rh_list, env) when is_list(rh_list) do
    {items, env} = compare_list(pattern, rh_list, env)
    {%ContainerDiff{type: :list, items: items}, env}
  end

  def cmp(%{val: {:{}, _, _}} = pattern, rh_tuple, env) when is_tuple(rh_tuple) do
    {items, env} = compare_tuple(pattern, rh_tuple, env)
    {%ContainerDiff{type: :tuple, items: items}, env}
  end

  def cmp(%{val: {_, _}} = pattern, rh_tuple, env) when is_tuple(rh_tuple) do
    {items, env} = compare_tuple(pattern, rh_tuple, env)
    {%ContainerDiff{type: :tuple, items: items}, env}
  end

  def cmp(%{val: {:%{}, _, _}} = pattern, rh_map, env) when is_map(rh_map) do
    {items, env} = compare_map(pattern, rh_map, env)
    {%ContainerDiff{type: :map, items: items}, env}
  end

  def cmp(%{val: {var, _, _}} = pattern, rh_value, {vars, pins} = env) when is_atom(var) do
    case Keyword.get(vars, var) do
      :ex_unit_unbound_var ->
        new_vars = Keyword.put(vars, var, rh_value)
        {%__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :eq
         }, {new_vars, pins}}
      ^rh_value ->
        {%__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :eq
         }, env}
      other_value ->
        {%__MODULE__{
            type: :value,
            lh: pattern,
            rh: rh_value,
            diff_result: :neq
         }, env}
    end
  end

  def cmp(%{val: lh_value} = pattern, rh_value, env) when is_integer(lh_value) and is_integer(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def cmp(%{val: lh_value} = pattern, rh_value, env) when is_float(lh_value) and is_float(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def cmp(%{val: lh_value} = pattern, rh_value, env) when is_binary(lh_value) and is_binary(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def cmp(%{val: lh_value} = pattern, rh_value, env) when is_atom(lh_value) and is_atom(rh_value) do
    like_value_compare(pattern, rh_value, env)
  end

  def cmp(%{val: _lh_value} = pattern, rh_value, env) do
    {%__MODULE__{
      type: :different,
      lh: pattern,
      rh: rh_value,
      diff_result: :neq
    }, env}
  end
 
  def cmp(@no_value, rh_value, env) do
    {%__MODULE__{
      type: :different,
      lh: @no_value,
      rh: rh_value,
      diff_result: :neq
    }, env}
  end

  defp compare_list(%{val: [{:|, _, [head, tail]} | _rest]} = pattern, [rh_head | rh_tail], env) do
    head_pattern = Pattern.new(head, pattern, :cons_l)
    tail_pattern = Pattern.new(tail, pattern)
    {h, env} = cmp(head_pattern, rh_head, env)
    {t, env} = cmp(tail_pattern, rh_tail, env)
    {[h, t], env}
  end

  defp compare_list(pattern, rh, env) do
    patterns = Enum.map(pattern.val, &(Pattern.new(&1, pattern)))
    compare_list_items(patterns, rh, env)
  end

  defp compare_list_items([lh_h | lh_rest], [rh_h | rh_rest], env) do
    {h, env} = cmp(lh_h, rh_h, env)
    {t, env} = compare_list_items(lh_rest, rh_rest, env)
    {[h | t], env}
  end

  defp compare_list_items([], [], env), do: {[], env}
  defp compare_list_items([lh_h | lh_rest], [], env) do
    {h, env} = cmp(lh_h, @no_value, env)
    {t, env} = compare_list_items(lh_rest, [], env)
    {[h | t], env}
  end
  defp compare_list_items([], [rh_h | rh_rest], env) do
    {h, env} = cmp(@no_value, rh_h, env)
    {t, env} = compare_list_items([], rh_rest, env)
    {[h | t], env}
  end

  defp compare_tuple(%{val: {_, _}} = pattern, rh_tuple, env) do
    patterns = pattern.val
    |> Tuple.to_list()
    |> Enum.map(&(Pattern.new(&1, pattern)))

    compare_list_items(patterns, Tuple.to_list(rh_tuple), env)
  end

  defp compare_tuple(%{val: {:{}, _, members}} = pattern, rh_tuple, env) do
    patterns = Enum.map(members, &(Pattern.new(&1, pattern)))
    compare_list_items(patterns, Tuple.to_list(rh_tuple), env)
  end

  defp compare_map(%{val: {:%{}, _, members}} = pattern, rh_map, env) do
    compare_map_items(pattern, members, rh_map, env)
  end

  defp compare_map_items(pattern, [lh_h | lh_t], rh_map, env) do
    {key, lh_value} = lh_h
    {rh_value, rh_map} =
      case Map.pop(rh_map, key) do
        {nil, rh_map} ->
          {@no_value, rh_map}
        {val, map} ->
          {{key, val}, map}
      end
    {h, env} = cmp(Pattern.new({key, lh_value}, pattern), rh_value, env)
    {t, env} = compare_map_items(pattern, lh_t, rh_map, env)
    {[ h | t ], env}
  end

  defp compare_map_items(_pattern, [], rh_map, env) when rh_map == %{} do
    {[], env}
  end

  defp compare_map_items(pattern, [], rh_map, env) do
    lh = @no_value
    rh_key = rh_map
      |> Map.keys
      |> List.first()

    {rh_value, rh_map} = Map.pop(rh_map, rh_key)
    {h, env} = cmp(lh, {rh_key, rh_value}, env)
    {t, env} = compare_map_items(pattern, [], rh_map, env)
    {[h | t], env}
  end

  defp like_value_compare(%{val: lh_value} = pattern, rh_value, env) do
      result = if lh_value == rh_value, do: :eq, else: :neq 
      {%__MODULE__{
        type: :value,
        lh: pattern,
        rh: rh_value,
        diff_result: result
      }, env}
  end

end
