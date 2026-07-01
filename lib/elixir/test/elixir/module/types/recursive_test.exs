# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.RecursiveTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr

  describe "recursive types" do
    defp recursive_node(descr) do
      recursive(%{X: fn _recur -> descr end})
      |> Map.fetch!(:X)
    end

    defp assert_finishes(fun) do
      task = Task.async(fun)

      case Task.yield(task, 1_000) || Task.shutdown(task, :brutal_kill) do
        {:ok, result} -> result
        nil -> flunk("operation did not finish")
      end
    end

    # linked_list(T) = {T, linked_list(T)} | nil
    defp linked_list(descr) do
      recursive(%{
        X: fn recur -> tuple([descr, recur.(:X)]) |> bare_union(atom([nil])) end
      })
      |> Map.fetch!(:X)
      |> to_descr()
    end

    defp int_linked_list(), do: linked_list(integer())
    defp number_linked_list(), do: linked_list(opt_union(integer(), float()))

    # binary_tree(T) = {T, binary_tree(T), binary_tree(T)} | nil
    defp binary_tree(descr) do
      recursive(%{
        T: fn recur ->
          tuple([descr, recur.(:T), recur.(:T)])
          |> bare_union(atom([nil]))
        end
      })
      |> Map.fetch!(:T)
      |> to_descr()
    end

    test "node infrastructure" do
      descr = integer()

      assert to_descr(recursive_node(descr)) == descr

      assert to_descr(:term) == :term

      d = opt_union(integer(), atom())
      assert to_descr(d) == d

      node =
        recursive(%{
          X: fn _ -> integer() end
        })
        |> Map.fetch!(:X)

      assert to_descr(node) == integer()

      d = opt_union(tuple([integer(), atom()]), list(float()))
      assert to_descr(recursive_node(d)) == d
    end

    test "constructors accept nodes and descrs" do
      n1 = recursive_node(integer())
      n2 = recursive_node(atom())

      result = tuple([n1, n2])
      refute empty?(result)

      n = recursive_node(integer())
      result = tuple([n, float()])
      assert equal?(result, tuple([integer(), float()]))

      n = recursive_node(integer())
      from_node = list(n)
      from_descr = list(integer())
      assert equal?(from_node, from_descr)

      elem_node = recursive_node(integer())
      tail_node = recursive_node(empty_list())

      from_nodes = non_empty_list(elem_node, tail_node)
      from_descrs = non_empty_list(integer(), empty_list())
      assert equal?(from_nodes, from_descrs)

      n = recursive_node(integer())
      result = closed_map(a: n)
      refute empty?(result)

      n = recursive_node(atom())
      result = open_map(b: n)
      refute empty?(result)
    end

    test "real-world types" do
      ## integer linked list: X = {integer(), X} | nil
      int_list = int_linked_list()
      assert is_map_key(int_list, :tuple) and is_map_key(int_list, :atom)

      # {integer(), nil} <: X
      assert subtype?(tuple([integer(), atom([nil])]), int_list)
      # nil <: X
      assert subtype?(atom([nil]), int_list)
      # {integer(), {integer(), nil}} <: X
      assert subtype?(tuple([integer(), tuple([integer(), atom([nil])])]), int_list)
      # {integer(), integer()} </: X
      refute subtype?(tuple([integer(), integer()]), int_list)

      # Y = {number(), Y} | nil — integer list <: number list
      number_list = number_linked_list()
      assert subtype?(int_list, number_list)
      refute subtype?(number_list, int_list)

      ## json_value recursive type
      # json_value = nil | boolean() | number() | String.t() | [json_value()]
      #            | %{optional(String.t()) => json_value}
      %{Json: json_value_node} =
        recursive(%{
          Json: fn recur ->
            scalar =
              atom([nil])
              |> bare_union(boolean())
              |> bare_union(integer())
              |> bare_union(float())
              |> bare_union(binary())

            array = list(recur.(:Json))
            object = open_map([{to_domain_keys(binary()), recur.(:Json)}])

            scalar
            |> bare_union(array)
            |> bare_union(object)
          end
        })

      json_value = to_descr(json_value_node)

      assert subtype?(atom([nil]), json_value)
      assert subtype?(list(binary()), json_value)
      assert subtype?(empty_map(), json_value)
      refute subtype?(pid(), json_value)

      ## IO.chardata recursive type
      %{Char: chardata_node} =
        recursive(%{
          Char: fn recur ->
            head = integer() |> bare_union(binary())
            tail = recur.(:Char)

            binary()
            |> bare_union(empty_list())
            |> bare_union(non_empty_list(head, tail))
          end
        })

      chardata = to_descr(chardata_node)

      assert subtype?(binary(), chardata)
      assert subtype?(empty_list(), chardata)
      assert subtype?(non_empty_list(integer(), empty_list()), chardata)
      assert subtype?(non_empty_list(binary(), binary()), chardata)
      refute subtype?(pid(), chardata)

      ## expression trees
      # Expr = integer() | {atom, Expr, Expr}, Binop = {atom, Expr, Expr}
      %{Expr: nexpr_node, Binop: nbinop_node} =
        recursive(%{
          Expr: fn recur ->
            bare_union(
              integer(),
              tuple([
                atom(),
                recur.(:Expr),
                recur.(:Expr)
              ])
            )
          end,
          Binop: fn recur ->
            tuple([atom(), recur.(:Expr), recur.(:Expr)])
          end
        })

      texpr = to_descr(nexpr_node)
      tbinop = to_descr(nbinop_node)

      refute empty?(texpr)
      refute empty?(tbinop)

      # 42 is an Expr
      assert subtype?(integer(), texpr)

      # {:+, 1, 2} is an Expr and a Binop
      assert subtype?(tuple([atom(), integer(), integer()]), texpr)
      assert subtype?(tuple([atom(), integer(), integer()]), tbinop)

      # {:*, {:+, 1, 2}, 3} is an Expr
      inner = tuple([atom(), integer(), integer()])
      assert subtype?(tuple([atom(), inner, integer()]), texpr)
    end

    test "emptiness" do
      # X = {X, X} is empty (no base case)
      %{X: nx} = recursive(%{X: fn recur -> tuple([recur.(:X), recur.(:X)]) end})
      tx = to_descr(nx)

      assert empty?(tx)

      # X = {X} is empty, so {X} is empty too
      %{X: nx} = recursive(%{X: fn recur -> tuple([recur.(:X)]) end})
      tx = to_descr(nx)
      ttx = tuple([tx])

      assert empty?(tx)
      assert empty?(ttx)

      # X = {integer()} | {X, X} has base case, not empty
      %{X: nx} =
        recursive(%{
          X: fn recur -> bare_union(tuple([integer()]), tuple([recur.(:X), recur.(:X)])) end
        })

      tx = to_descr(nx)

      refute empty?(tx)

      ## mutual recursion
      # X = {int,Y} | nil, Y = {bool,X} | nil: both not empty
      %{X: node_x, Y: node_y} =
        recursive(%{
          X: fn recur -> tuple([integer(), recur.(:Y)]) |> bare_union(atom([nil])) end,
          Y: fn recur -> tuple([boolean(), recur.(:X)]) |> bare_union(atom([nil])) end
        })

      tx = to_descr(node_x)
      ty = to_descr(node_y)

      refute empty?(tx)
      refute empty?(ty)

      # {int, {bool, nil}} <: X
      inner_y = tuple([boolean(), atom([nil])])
      assert subtype?(tuple([integer(), inner_y]), tx)

      # {bool, {int, nil}} <: Y
      inner_x = tuple([integer(), atom([nil])])
      assert subtype?(tuple([boolean(), inner_x]), ty)

      # {int, {bool, {int, nil}}} <: X
      level3 = tuple([integer(), atom([nil])])
      level2 = tuple([boolean(), level3])
      assert subtype?(tuple([integer(), level2]), tx)

      # X = {Y}|nil, Y = {X}: X not empty, Y not empty (Y can hold {nil})
      %{X: nx, Y: ny} =
        recursive(%{
          X: fn recur -> tuple([recur.(:Y)]) |> bare_union(atom([nil])) end,
          Y: fn recur -> tuple([recur.(:X)]) end
        })

      tx = to_descr(nx)
      ty = to_descr(ny)
      refute empty?(tx)
      refute empty?(ty)

      ## cycle detection
      cases = [
        # X -> {Y}, Y -> {Z}, Z -> {X}
        {"3-cycle no base", true,
         %{
           X: fn recur -> tuple([recur.(:Y)]) end,
           Y: fn recur -> tuple([recur.(:Z)]) end,
           Z: fn recur -> tuple([recur.(:X)]) end
         }},
        # X -> {Y}, Y -> {Z}, Z -> {X} or nil
        {"3-cycle with base", false,
         %{
           X: fn recur -> tuple([recur.(:Y)]) end,
           Y: fn recur -> tuple([recur.(:Z)]) end,
           Z: fn recur -> tuple([recur.(:X)]) |> bare_union(atom([nil])) end
         }}
      ]

      for {desc, expected_empty, gen} <- cases do
        nodes = recursive(gen)

        for {_key, node} <- nodes do
          t = to_descr(node)

          if expected_empty do
            assert empty?(t), "#{desc}: expected empty but wasn't"
          else
            refute empty?(t), "#{desc}: expected not empty but was"
          end
        end
      end

      ## binary trees
      # Tree = {atom, Tree, Tree} | nil: not empty
      refute empty?(binary_tree(atom()))

      ## list-head recursion with base case is not empty
      # X = non_empty_list(X, []) | non_empty_list(integer(), [])
      %{X: nx} =
        recursive(%{
          X: fn recur ->
            non_empty_list(recur.(:X), empty_list())
            |> bare_union(non_empty_list(integer(), empty_list()))
          end
        })

      tx = to_descr(nx)
      refute empty?(tx)
    end

    test "recursive map domains" do
      # X = %{atom() => X}
      %{X: node} =
        recursive(%{
          X: fn recur -> open_map([{to_domain_keys(atom()), recur.(:X)}]) end
        })

      tx = to_descr(node)
      rebuilt = open_map([{to_domain_keys(atom()), tx}])
      closed = closed_map([{to_domain_keys(atom()), tx}])

      assert assert_finishes(fn -> subtype?(tx, rebuilt) end)
      refute assert_finishes(fn -> empty?(opt_intersection(tx, rebuilt)) end)
      refute assert_finishes(fn -> subtype?(tx, closed) end)
      refute assert_finishes(fn -> disjoint?(tx, closed) end)
    end

    test "subtyping" do
      ## recursion on list tail
      # X = non_empty_list(integer(), X) | []
      tx =
        recursive(%{
          X: fn recur -> non_empty_list(integer(), recur.(:X)) |> bare_union(empty_list()) end
        })
        |> Map.fetch!(:X)
        |> to_descr()

      # [] <: X
      assert subtype?(empty_list(), tx)

      # non_empty_list(integer()) <: X (terminates with [])
      assert subtype?(non_empty_list(integer()), tx)

      ## binary trees
      int_bin_tree = binary_tree(integer())

      # nil is a valid tree
      assert subtype?(atom([nil]), int_bin_tree)

      # {42, nil, nil} is a valid tree
      assert subtype?(tuple([integer(), atom([nil]), atom([nil])]), int_bin_tree)

      # {1, {2, nil, nil}, {3, nil, nil}} is a valid tree
      leaf = tuple([integer(), atom([nil]), atom([nil])])
      assert subtype?(tuple([integer(), leaf, leaf]), int_bin_tree)

      # wrong arity: {1, nil} not a subtype
      refute subtype?(tuple([integer(), atom([nil])]), int_bin_tree)

      # wrong tag type: {float, nil, nil} not a subtype
      refute subtype?(tuple([float(), atom([nil]), atom([nil])]), int_bin_tree)

      # X = non_empty_list(integer(), X) | non_empty_list(integer()) | []
      # Y = list(integer())
      gen = %{
        X: fn recur ->
          non_empty_list(integer(), recur.(:X))
          |> bare_union(non_empty_list(integer(), empty_list()))
          |> bare_union(empty_list())
        end
      }

      tx = recursive(gen)[:X] |> to_descr()
      assert equal?(tx, list(integer()))

      # X = %{outer: %{inner: X}} | nil
      # Y = %{outer: %{inner: Y}} | %{outer: %{inner: integer}} | nil
      %{X: nx, Y: ny} =
        recursive(%{
          X: fn recur ->
            closed_map(outer: closed_map(inner: recur.(:X))) |> bare_union(atom([nil]))
          end,
          Y: fn recur ->
            closed_map(outer: closed_map(inner: recur.(:Y)))
            |> bare_union(closed_map(outer: closed_map(inner: integer())))
            |> bare_union(atom([nil]))
          end
        })

      tx = to_descr(nx)
      ty = to_descr(ny)
      assert subtype?(tx, ty), "nested map wrapping map with recursive field"

      # X = nil | {integer, X}
      # Y = nil | {integer or float, Y}
      %{X: nx, Y: ny} =
        recursive(%{
          X: fn recur -> tuple([integer(), recur.(:X)]) |> bare_union(atom([nil])) end,
          Y: fn recur ->
            tuple([bare_union(integer(), float()), recur.(:Y)]) |> bare_union(atom([nil]))
          end
        })

      tx = to_descr(nx)
      ty = to_descr(ny)
      assert subtype?(tx, ty)
    end

    test "set operations accept nodes" do
      n = recursive_node(integer())
      assert equal?(bare_union(n, float()), bare_union(integer(), float()))
      assert equal?(bare_union(atom(), n), bare_union(atom(), integer()))
      assert equal?(opt_union(n, float()), bare_union(integer(), float()))

      n = recursive_node(bare_union(integer(), atom()))
      assert equal?(bare_intersection(n, integer()), integer())
      assert equal?(opt_intersection(n, integer()), integer())
      assert equal?(opt_intersection(atom(), n), atom())

      n = recursive_node(bare_union(integer(), float()))
      float_node = recursive_node(float())
      assert equal?(bare_difference(n, float()), integer())
      assert equal?(opt_difference(n, float()), integer())
      assert equal?(opt_difference(bare_union(integer(), float()), float_node), integer())
      assert equal?(opt_negation(float_node), opt_negation(float()))
    end

    test "set operations on nodes built with constructors" do
      ## map intersection inside nodes
      # X = %{a: %{a: X}} | %{a: %{a: atom()}}
      # Y = %{a: %{a: X}} | %{a: %{a: atom()}}
      %{X: nx, Y: ny} =
        recursive(%{
          X: fn recur ->
            closed_map(a: closed_map(a: recur.(:X)))
            |> bare_union(closed_map(a: closed_map(a: atom())))
          end,
          Y: fn recur ->
            closed_map(a: closed_map(a: recur.(:Y)))
            |> bare_union(closed_map(a: closed_map(a: atom())))
          end
        })

      tx = to_descr(nx)
      ty = to_descr(ny)

      refute empty?(opt_intersection(tx, ty))
      assert equal?(opt_union(tx, ty), tx)

      # X = {{X}} | {{atom()}}
      # Y = {{Y}} | {{atom()}}
      %{X: nx, Y: ny} =
        recursive(%{
          X: fn recur -> tuple([tuple([recur.(:X)])]) |> bare_union(tuple([tuple([atom()])])) end,
          Y: fn recur -> tuple([tuple([recur.(:Y)])]) |> bare_union(tuple([tuple([atom()])])) end
        })

      tx = to_descr(nx)
      ty = to_descr(ny)
      refute empty?(opt_intersection(tx, ty))
      assert equal?(opt_union(tx, ty), tx)
    end

    test "type operators" do
      ## tuple operators on descr with recursive node element
      # X = {integer(), X} | {integer(), atom()}
      %{X: nx} =
        recursive(%{
          X: fn recur ->
            tuple([integer(), recur.(:X)]) |> bare_union(tuple([integer(), atom()]))
          end
        })

      t = to_descr(nx)

      assert {false, type} = tuple_fetch(t, 0)
      assert equal?(type, integer())
      assert {false, _type} = tuple_fetch(t, 1)

      result = tuple_values(t)
      assert subtype?(integer(), result)

      result = tuple_delete_at(t, 0)
      assert {false, _type} = tuple_fetch(result, 0)

      result = tuple_insert_at(t, 0, boolean())
      assert {false, type} = tuple_fetch(result, 0)
      assert equal?(type, boolean())

      # X = {X} | {atom()}
      %{X: nx} =
        recursive(%{X: fn recur -> tuple([recur.(:X)]) |> bare_union(tuple([atom()])) end})

      tx = to_descr(nx)
      t = opt_difference(tx, tuple([atom()]))
      assert {false, type} = tuple_fetch(t, 0)
      assert equal?(type, tx)

      ## map_fetch_key on descr with recursive node value
      # X = %{a: integer(), b: X} | %{a: integer(), b: atom()}
      %{X: nx} =
        recursive(%{
          X: fn recur ->
            closed_map(a: integer(), b: recur.(:X))
            |> bare_union(closed_map(a: integer(), b: atom()))
          end
        })

      assert {false, type} = map_fetch_key(to_descr(nx), :a)
      assert equal?(type, integer())
      assert {false, _type} = map_fetch_key(to_descr(nx), :b)

      ## list_hd and list_tl on descr with recursive node tail
      # X = non_empty_list(integer(), X) | non_empty_list(integer(), [])
      %{X: nx} =
        recursive(%{
          X: fn recur ->
            non_empty_list(integer(), recur.(:X))
            |> bare_union(non_empty_list(integer(), empty_list()))
          end
        })

      assert {:ok, type} = list_hd(to_descr(nx))
      assert equal?(type, integer())
      assert {:ok, _type} = list_tl(to_descr(nx))

      ## list_hd on descr with recursive node as head element
      # X = non_empty_list(X, []) | non_empty_list(atom(), [])
      %{X: nx} =
        recursive(%{
          X: fn recur ->
            non_empty_list(recur.(:X), empty_list())
            |> bare_union(non_empty_list(atom(), empty_list()))
          end
        })

      assert {:ok, _type} = list_hd(to_descr(nx))

      ## fun_apply with recursive node argument
      # X = {X} | {atom()}
      %{X: nx} =
        recursive(%{
          X: fn recur -> tuple([recur.(:X)]) |> bare_union(tuple([atom()])) end
        })

      assert {:ok, type} = fun_apply(fun([term()], integer()), [nx])
      assert equal?(type, integer())
    end
  end
end
