Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.RecordRewriterTest do
  use ExUnit.Case, async: true

  ## Helpers

  defmacrop clause(expr) do
    quote do: extract_clause(unquote(Macro.escape(expr)))
  end

  defp extract_clause(fun) do
    { { :fun, _, { :clauses, clauses } }, _ } =
      :elixir_translator.translate_each(fun, :elixir.scope_for_eval([]))
    hd(clauses)
  end

  defp optimize_clause(clause) do
    { clause, dict, res } = Kernel.RecordRewriter.optimize_clause(clause)
    dict = Enum.map dict, fn { k, { v, _ } } -> { k, v }; other -> other; end
    { clause, dict, res }
  end

  ## Dictionary tests

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
    clause = clause(fn({ x, y } = { Macro.Env[], Range[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range], nil }

    clause = clause(fn({ Macro.Env[], y } = { x, Range[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range], nil }
  end

  test "inside body" do
    clause = clause(fn(x = Macro.Env[]) -> y = x end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Macro.Env], { Macro.Env, nil } }
  end

  test "inside body with variable overridden" do
    clause = clause(fn(x = Macro.Env[], y = Range[]) -> y = x end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range, "y@1": Macro.Env], { Macro.Env, nil } }
  end

  test "inside body with nested tuple" do
    clause = clause(fn(x = Range[]) -> ^x = Range[first: { :hello, 2 }] end)
    assert optimize_clause(clause) == { clause, [x: Range], { Range, [nil, { :hello, nil }, nil] } }
  end

  test "conflicting definition" do
    clause = clause(fn(x = Macro.Env[]) -> ^x = Range[]; :foo end)
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
    clause = clause(fn -> (x = Macro.Env[]).(y = Range[]) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range], nil }
  end

  test "inside remote call" do
    clause = clause(fn -> (x = Macro.Env[]).call(y = Range[]) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range], nil }
  end

  test "inside list comprehension" do
    clause = clause(fn -> lc x = Macro.Env[] inlist sample, do: x end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "inside bit comprehension" do
    clause = clause(fn -> bc x = Macro.Env[] inbits sample, do: <<x>> end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "inside function retrieval" do
    clause = clause(fn -> function(x = Macro.Env[], y, z) end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], nil }
  end

  test "inside anonymous function" do
    clause = clause(fn -> fn (x = Macro.Env[]) -> x end end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "inside case" do
    clause = clause(fn -> case x = Macro.Env[] do _ -> x end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> case something do x = Macro.Env[] -> x; Macro.Env[] = x -> x end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> case something do x = Macro.Env[] -> x; Macro.Env[] = y -> y end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }

    clause = clause(fn -> case something do 1 -> x = Macro.Env[]; 2 -> x = Macro.Env[] end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> case something do 1 -> x = Macro.Env[]; 2 -> x = Range[] end end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }
  end

  test "inside case with nested tuple" do
    clause = clause(fn -> case something do x = Range[first: { :foo, 2 }] -> x; Range[] = x -> x end end)
    assert optimize_clause(clause) == { clause, [x: Range], { Range, nil } }

    clause = clause(fn -> case something do x = Range[first: { :foo, 2 }] -> x; Range[first: { :foo, 2 }] = x -> x end end)
    assert optimize_clause(clause) == { clause, [x: Range], { Range, [nil, { :foo, nil }, nil] } }
  end

  test "inside receive" do
    clause = clause(fn -> receive do x = Macro.Env[] -> x; Macro.Env[] = x -> x end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> receive do x = Macro.Env[] -> x; Macro.Env[] = y -> y end end)
    assert optimize_clause(clause) == { clause, [], { Macro.Env, nil } }

    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; 2 -> x = Macro.Env[] end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; 2 -> x = Range[] end end)
    assert optimize_clause(clause) == { clause, [x: nil], nil }
  end

  test "inside receive with after" do
    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; after 2 -> x = Macro.Env[]; end end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env], { Macro.Env, nil } }

    clause = clause(fn -> receive do 1 -> x = Macro.Env[]; after 2 -> x = Range[]; end end)
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
end
