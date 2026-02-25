# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.HelpersTest do
  use ExUnit.Case, async: true
  import Module.Types.Helpers

  describe "expr_to_string/1" do
    test "common expressions" do
      assert expr_to_string({1, 2}) == "{1, 2}"
      assert expr_to_string(quote(do: Foo.bar(arg))) == "Foo.bar(arg)"
    end

    test "rewrites" do
      assert expr_to_string(quote(do: :erlang.band(a, b))) == "Bitwise.band(a, b)"
      assert expr_to_string(quote(do: :erlang.orelse(a, b))) == "a or b"
      assert expr_to_string(quote(do: :erlang."=:="(a, b))) == "a === b"
      assert expr_to_string(quote(do: :erlang.list_to_atom(a))) == "List.to_atom(a)"
      assert expr_to_string(quote(do: :maps.remove(a, b))) == "Map.delete(b, a)"
      assert expr_to_string(quote(do: :erlang.element(1, a))) == "elem(a, 0)"
      assert expr_to_string(quote(do: :erlang.element(:erlang.+(a, 1), b))) == "elem(b, a)"
    end

    test "Kernel macros" do
      case = Macro.expand(quote(do: if(condition, do: :this, else: :that)), __ENV__)
      assert expr_to_string(case) == "if condition do\n  :this\nelse\n  :that\nend"

      case = Macro.expand(quote(do: :this || :that), __ENV__)
      assert expr_to_string(case) == ":this || :that"

      case = Macro.expand(quote(do: :this && :that), __ENV__)
      assert expr_to_string(case) == ":this && :that"

      case = Macro.expand(quote(do: !expr), __ENV__)
      assert expr_to_string(case) == "!expr"
    end

    test "case/try/receive/cond" do
      assert expr_to_string(
               quote do
                 case expr do
                   :this -> :this!
                   :that -> :that!
                 end
               end
             ) == "case expr do\n  ...\nend"

      assert expr_to_string(
               quote do
                 try do
                   :this -> :this!
                   :that -> :that!
                 rescue
                   _ ->
                     nil
                 end
               end
             ) == "try do\n  ...\nend"

      assert expr_to_string(
               quote do
                 cond do
                   :this -> :this!
                   :that -> :that!
                 end
               end
             ) == "cond do\n  ...\nend"

      assert expr_to_string(
               quote do
                 receive do
                   :this -> :this!
                   :that -> :that!
                 end
               end
             ) == "receive do\n  ...\nend"
    end
  end
end
