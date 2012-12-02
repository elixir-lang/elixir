Code.require_file "../../test_helper.exs", __FILE__

defmodule Kernel.RecordRewriterTest do
  use ExUnit.Case, async: true

  import Kernel.RecordRewriter

  ## Helpers

  defmacrop clause(expr) do
    quote do: extract_clause(unquote(Macro.escape(expr)))
  end

  defp extract_clause(fun) do
    { { :fun, _, { :clauses, clauses } }, _ } =
      :elixir_translator.translate_each(fun, :elixir.scope_for_eval([]))
    hd(clauses)
  end

  ## Dictionary tests

  test "simple atom" do
    clause = clause(fn -> :foo end)
    assert optimize_clause(clause) == { clause, [], nil }
  end

  test "with left-side arg match" do
    clause = clause(fn(arg = Macro.Env[]) -> :foo end)
    assert optimize_clause(clause) == { clause, [arg: Macro.Env], nil }
  end

  test "with right-side arg match" do
    clause = clause(fn(Macro.Env[] = arg) -> :foo end)
    assert optimize_clause(clause) == { clause, [arg: Macro.Env], nil }
  end

  test "with nested left-side arg match" do
    clause = clause(fn(arg = other = Macro.Env[]) -> :foo end)
    assert optimize_clause(clause) == { clause, [arg: Macro.Env, other: Macro.Env], nil }
  end

  test "with nested right-side arg match" do
    clause = clause(fn(Macro.Env[] = arg = other) -> :foo end)
    assert optimize_clause(clause) == { clause, [arg: Macro.Env, other: Macro.Env], nil }
  end

  test "with deep left-side arg match" do
    clause = clause(fn({ x, arg = Macro.Env[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [arg: Macro.Env], nil }
  end

  test "with deep right-side arg match" do
    clause = clause(fn({ x, Macro.Env[] = arg }) -> :foo end)
    assert optimize_clause(clause) == { clause, [arg: Macro.Env], nil }
  end

  test "with tuple match" do
    clause = clause(fn({ x, y } = { Macro.Env[], Range[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range], nil }

    clause = clause(fn({ Macro.Env[], y } = { x, Range[] }) -> :foo end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range], nil }
  end

  test "with body variable" do
    clause = clause(fn(x = Macro.Env[]) -> y = x end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Macro.Env], { Macro.Env, nil } }
  end

  test "with body variable overridden" do
    clause = clause(fn(x = Macro.Env[], y = Range[]) -> y = x end)
    assert optimize_clause(clause) == { clause, [x: Macro.Env, y: Range, "y@1": Macro.Env], { Macro.Env, nil } }
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
end
