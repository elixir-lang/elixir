Code.require_file("../test_helper.exs", __DIR__)

defmodule Kernel.SpecialFormsTest do
  use ExUnit.Case, async: true

  doctest Kernel.SpecialForms

  describe "cond" do
    test "does not leak variables for one clause" do
      x = 0

      cond do
        true ->
          x = 1
          x
      end

      assert x == 0
    end

    test "does not leak variables for one clause with non-boolean as catch-all" do
      x = 0

      cond do
        :otherwise ->
          x = 1
          x
      end

      assert x == 0
    end

    test "does not leak variables for multiple clauses" do
      x = 0

      cond do
        List.flatten([]) == [] ->
          x = 1
          x

        true ->
          x = 1
          x
      end

      assert x == 0
    end

    test "does not leak variables from conditions" do
      x = :not_nil

      result =
        cond do
          x = List.first([]) ->
            x

          true ->
            x
        end

      assert result == :not_nil
    end

    test "does not warn on non-boolean as catch-all" do
      cond do
        List.flatten([]) == [] -> :good
        :otherwise -> :also_good
      end
    end

    def false_fun(), do: false

    test "cond_clause error keeps line number in stacktrace" do
      try do
        cond do
          false_fun() -> :ok
        end
      rescue
        _ ->
          assert [{Kernel.SpecialFormsTest, _, _, meta} | _] = __STACKTRACE__
          assert meta[:file]
          assert meta[:line]
      end
    end
  end
end
