# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.MapTest do
  # Tests for the Map module
  use ExUnit.Case, async: true

  import TypeHelper
  import Module.Types.Descr
  defmacro domain_key(arg) when is_atom(arg), do: [arg]

  describe "Map.to_list/1" do
    test "checking" do
      assert typecheck!([x = %{}], Map.to_list(x)) == dynamic(list(tuple([term(), term()])))

      assert typecheck!(
               (
                 x = %{}
                 Map.to_list(x)
               )
             ) == empty_list()

      assert typecheck!(
               (
                 x = %{"c" => :three}
                 Map.to_list(x)
               )
             ) ==
               list(tuple([binary(), atom([:three])]))

      assert typecheck!(
               (
                 x = %{a: 1, b: "two"}
                 Map.to_list(x)
               )
             ) ==
               non_empty_list(
                 union(tuple([atom([:a]), integer()]), tuple([atom([:b]), binary()]))
               )

      assert typecheck!(
               (
                 x = %{"c" => :three, a: 1, b: "two"}
                 Map.to_list(x)
               )
             ) ==
               non_empty_list(
                 tuple([atom([:a]), integer()])
                 |> union(tuple([atom([:b]), binary()]))
                 |> union(tuple([binary(), atom([:three])]))
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.to_list(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.to_list(x)) =~ "incompatible types given to Map.to_list/1"
    end
  end

  describe "Map.keys/1" do
    test "checking" do
      assert typecheck!([x = %{}], Map.keys(x)) == dynamic(list(term()))

      assert typecheck!(
               (
                 x = %{}
                 Map.keys(x)
               )
             ) == empty_list()

      assert typecheck!(
               (
                 x = %{"c" => :three}
                 Map.keys(x)
               )
             ) ==
               list(binary())

      assert typecheck!(
               (
                 x = %{a: 1, b: "two"}
                 Map.keys(x)
               )
             ) ==
               non_empty_list(union(atom([:a]), atom([:b])))

      assert typecheck!(
               (
                 x = %{"c" => :three, a: 1, b: "two"}
                 Map.keys(x)
               )
             ) ==
               non_empty_list(
                 atom([:a])
                 |> union(atom([:b]))
                 |> union(binary())
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.keys(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.keys(x)) =~ "incompatible types given to Map.keys/1"
    end
  end

  describe "Map.values/1" do
    test "checking" do
      assert typecheck!([x = %{}], Map.values(x)) == dynamic(list(term()))

      assert typecheck!(
               (
                 x = %{}
                 Map.values(x)
               )
             ) == empty_list()

      assert typecheck!(
               (
                 x = %{"c" => :three}
                 Map.values(x)
               )
             ) ==
               list(atom([:three]))

      assert typecheck!(
               (
                 x = %{a: 1, b: "two"}
                 Map.values(x)
               )
             ) ==
               non_empty_list(union(integer(), binary()))

      assert typecheck!(
               (
                 x = %{"c" => :three, a: 1, b: "two"}
                 Map.values(x)
               )
             ) ==
               non_empty_list(
                 integer()
                 |> union(binary())
                 |> union(atom([:three]))
               )
    end

    test "inference" do
      assert typecheck!(
               [x],
               (
                 _ = Map.values(x)
                 x
               )
             ) == dynamic(open_map())
    end

    test "errors" do
      assert typeerror!([x = []], Map.values(x)) =~ "incompatible types given to Map.values/1"
    end
  end
end
