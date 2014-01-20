Code.require_file "../test_helper.exs", __DIR__

defrecord Rec, first: 0, last: 0

defrecord BadRec, first: 0, last: 0 do
  defoverridable [first: 1]

  def first(_) do
    :not_optimizable
  end
end

defmodule Kernel.RecordRewriterTest do
  use ExUnit.Case, async: true

  ## Helpers

  defmacrop clause(expr) do
    quote do: extract_clause(unquote(Macro.escape(expr)))
  end

  defp extract_clause(fun) do
    { { :fun, _, { :clauses, clauses } }, _, _ } =
      :elixir.quoted_to_erl(fun, :elixir.env_for_eval(delegate_locals_to: __MODULE__))
    hd(clauses)
  end

  defp optimize_clause(clause) do
    { clause, dict, res } = Kernel.RecordRewriter.optimize_clause(__MODULE__, clause)
    dict = Enum.map dict, fn { k, { v, _ } } -> { k, v }; other -> other; end
    { clause, dict, res }
  end

  ## Inference tests

  test "simple atom" do
    clause = clause(fn -> :foo end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "with left-side arg match" do
    clause = clause(fn(x = Macro.Env[]) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "with right-side arg match" do
    clause = clause(fn(Macro.Env[] = x) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "with nested left-side arg match" do
    clause = clause(fn(x = y = Macro.Env[]) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Macro.Env], nil }
  end

  test "with nested right-side arg match" do
    clause = clause(fn(Macro.Env[] = x = y) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Macro.Env], nil }
  end

  test "with deep left-side arg match" do
    clause = clause(fn({ x, y = Macro.Env[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [y: Macro.Env], nil }
  end

  test "with deep right-side arg match" do
    clause = clause(fn({ x, Macro.Env[] = y }) -> :foo end)
    assert optimize_clause(clause) == { clause, [y: Macro.Env], nil }
  end

  test "with tuple match" do
    clause = clause(fn({ x, y } = { Macro.Env[], Rec[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Rec], nil }

    clause = clause(fn({ Macro.Env[], y } = { x, Rec[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Rec], nil }
  end

  test "inside body" do
    clause = clause(fn(x = Macro.Env[]) -> y = x end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Macro.Env], { Macro.Env, nil } }
  end

  test "inside body with variable overridden" do
    clause = clause(fn(x = Macro.Env[], y = Rec[]) -> y = x end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Rec, "y@1": Macro.Env], { Macro.Env, nil } }
  end

  test "inside body with nested tuple" do
    clause = clause(fn(x = Rec[]) -> ^x = Rec[first: { :hello, 2 }] end)
    assert optimize_clause(clause) == { clause, [x: Rec], { Rec, [nil, { :hello, nil }, nil] } }
  end

  test "with setelement" do
    clause = clause(fn(x = Macro.Env[]) -> :erlang.setelement(2, x, :foo) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn(x = Macro.Env[]) -> :erlang.setelement(2, x, y = Macro.Env[]) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Macro.Env], { Macro.Env, nil } }

    # Not optimized with changing the first element
    clause = clause(fn(x = Macro.Env[]) -> :erlang.setelement(1, x, :foo) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "conflicting definition" do
    clause = clause(fn(x = Macro.Env[]) -> ^x = Rec[]; :foo end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }
  end

  test "inside list" do
    clause = clause(fn -> [x = Macro.Env[]]; :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "inside tuple" do
    clause = clause(fn -> { x = Macro.Env[] }; :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "inside bin" do
    clause = clause(fn -> << x = Macro.Env[] >>; :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "inside operator" do
    clause = clause(fn -> 1 + (x = Macro.Env[]); :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "inside block" do
    clause = clause(fn -> :foo; (x = Macro.Env[]; x) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }
  end

  test "inside local call" do
    clause = clause(fn -> (x = Macro.Env[]).(y = Rec[]) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Rec], nil }
  end

  test "inside remote call" do
    clause = clause(fn -> x.call(y = Rec[]) end)
    assert optimize_clause(clause) == { clause, [y: Rec], nil }
  end

  test "inside list comprehension" do
    clause = clause(fn -> lc x = Macro.Env[] inlist sample, do: x end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "inside anonymous function" do
    clause = clause(fn -> fn (x = Macro.Env[]) -> x end end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "inside case" do
    clause = clause(fn -> case x = Macro.Env[] do _ -> x end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> case something do x = Macro.Env[] -> x; Macro.Env[] = y -> y end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }

    clause = clause(fn -> case something do 1 -> x = Macro.Env[]; 2 -> x = Macro.Env[] end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> case something do 1 -> x = Macro.Env[]; 2 -> x = Rec[] end end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }

    clause = clause(fn -> case something do x = Macro.Env[] -> x; x = Rec[] -> x; _ -> :ok end end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "inside case with nested tuple" do
    clause = clause(fn -> case something do x = Rec[first: { :foo, 2 }] -> x = x; Rec[] = x -> x = x end end)
    assert optimize_clause(clause) == { clause, ["x@4": Rec], { Rec, nil } }

    clause = clause(fn -> case something do x = Rec[first: { :foo, 2 }] -> x = x; Rec[first: { :foo, 2 }] = x -> x = x end end)
    assert optimize_clause(clause) == { clause, ["x@4": Rec], { Rec, [nil, { :foo, nil }, nil] } }
  end

  test "empty receive" do
    clause = clause(fn -> receive do end end)
    assert optimize_clause(clause) == { clause, [], [] }
  end

  test "inside receive" do
    clause = clause(fn -> receive do x = Macro.Env[] -> x; Macro.Env[] = y -> y end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }

    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; 2 -> x = Macro.Env[] end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; 2 -> x = Rec[] end end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }
  end

  test "inside receive with after" do
    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; after 2 -> x = Macro.Env[]; end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; after 2 -> x = Rec[]; end end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }
  end

  test "inside try" do
    clause = clause(fn -> try do x = Macro.Env[]; x end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }

    clause = clause(fn -> try do x = Macro.Env[]; x; catch :oops -> x = Macro.Env[]; end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }

    clause = clause(fn -> try do x = Macro.Env[]; x; after x = Macro.Env[]; end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }
  end

  ## Rewrite tests

  test "getter call is rewriten" do
    { clause, rewriten } =
      { clause(fn(x = Rec[]) -> x.first end), clause(fn(x = Rec[]) -> :erlang.element(2, x) end) }

    assert optimize_clause(clause) == { rewriten, [x: Rec], nil }
  end

  test "direct getter call is rewriten" do
    { clause, rewriten } =
      { clause(fn() -> Rec[].first end), clause(fn() -> :erlang.element(2, Rec[]) end) }

    assert optimize_clause(clause) == { rewriten, [], nil }
  end

  test "setter call is rewriten" do
    { clause, rewriten } =
      { clause(fn(x = Rec[]) -> x.first(:first) end), clause(fn(x = Rec[]) -> :erlang.setelement(2, x, :first) end) }

    assert optimize_clause(clause) == { rewriten, [x: Rec], { Rec, nil } }
  end

  test "nested setter call is rewriten" do
    { clause, rewriten } =
      { clause(fn(x = Rec[]) -> x.first(:first).last(:last) end), clause(fn(x = Rec[]) -> :erlang.setelement(3, :erlang.setelement(2, x, :first), :last) end) }

    assert optimize_clause(clause) == { rewriten, [x: Rec], { Rec, nil } }
  end

  test "updater call is rewriten" do
    { clause, rewriten } =
      { clause(fn(x = Rec[]) -> x.update_first(&(&1 + 1)) end), clause(fn(x = Rec[]) -> Rec.update_first(&(&1 + 1), x) end) }
    assert optimize_clause(clause) == { rewriten, [x: Rec], { Rec, nil } }
  end

  test "update call is rewriten" do
    { clause, rewriten } =
      { clause(fn(x = Rec[]) -> x.update(first: 1) end), clause(fn(x = Rec[]) -> Rec.update([first: 1], x) end) }
    assert optimize_clause(clause) == { rewriten, [x: Rec], { Rec, nil } }
  end

  test "fallback for unknown fields" do
    { clause, rewriten } =
      { clause(fn(x = Rec[]) -> x.unknown(1, 2) end), clause(fn(x = Rec[]) -> Rec.unknown(1, 2, x) end) }
    assert optimize_clause(clause) == { rewriten, [x: Rec], nil }
  end

  test "fallback for rewriten fields" do
    { clause, rewriten } =
      { clause(fn(x = BadRec[]) -> x.first end), clause(fn(x = BadRec[]) -> BadRec.first(x) end) }
    assert optimize_clause(clause) == { rewriten, [x: BadRec], nil }
  end

  test "noop for not records fields" do
    clause = clause(fn(x = { :not_a_record, _ }) -> x.unknown end)
    assert optimize_clause(clause) == { clause, [x: :not_a_record], nil }
  end

  test "noop for conflicting inference" do
    clause = clause(fn(x = Macro.Env[]) -> ^x = Rec[]; x.first end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }
  end
end
