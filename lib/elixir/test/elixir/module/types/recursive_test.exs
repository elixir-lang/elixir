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

    @finish_timeout 1_000
    # In words (~200MB); runaway recursion trips this well before the timeout.
    @finish_max_heap 25_000_000

    # Runs fun in a process with bounded heap and bounded time, so runaway
    # recursion fails cleanly instead of taking down the machine.
    # Returns {:ok, result} or {:error, message}.
    defp run_guarded(fun) do
      parent = self()
      ref = make_ref()

      {pid, mref} =
        spawn_monitor(fn ->
          Process.flag(:max_heap_size, %{size: @finish_max_heap, kill: true, error_logger: false})
          send(parent, {ref, fun.()})
        end)

      receive do
        {^ref, result} ->
          Process.demonitor(mref, [:flush])
          {:ok, result}

        {:DOWN, ^mref, :process, ^pid, reason} ->
          {:error, "operation died (#{inspect(reason)}), runaway recursion"}
      after
        @finish_timeout ->
          Process.exit(pid, :kill)

          receive do
            {:DOWN, ^mref, :process, ^pid, _} -> :ok
          end

          {:error, "operation did not finish within #{@finish_timeout}ms"}
      end
    end

    # Returns the result of fun, failing the test if it does not finish.
    defp assert_finishes(fun) do
      case run_guarded(fun) do
        {:ok, result} -> result
        {:error, message} -> flunk(message)
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

  describe "optimized set operations on recursive types" do
    # The opt_* operations recurse into tuple elements, map field values and
    # list heads/tails. When those contain recursive nodes, the operations must
    # detect that they are revisiting the same pair of BDDs and terminate.
    #
    # Loop-prone checks run through assert_finishes/1 so that a reintroduced
    # loop fails the test instead of exhausting the machine. Results are
    # checked against a closed-form expectation where one exists, and against
    # the bare_* operations (recursion-safe by construction) otherwise.

    ## Recursive type builders. Each call builds fresh nodes, so two calls
    ## produce alpha-equivalent types with distinct node identities.

    # X = {integer(), X} | nil, as a node
    defp int_list_node() do
      recursive(%{X: fn recur -> tuple([integer(), recur.(:X)]) |> bare_union(atom([nil])) end})
      |> Map.fetch!(:X)
    end

    # M = %{a: M} | nil, as a node
    defp rec_map_node() do
      recursive(%{M: fn recur -> closed_map(a: recur.(:M)) |> bare_union(atom([nil])) end})
      |> Map.fetch!(:M)
    end

    defp rec_map(), do: to_descr(rec_map_node())

    # M = %{a: M, b: descr} | nil
    defp rec_map_with(descr) do
      recursive(%{
        M: fn recur -> closed_map(a: recur.(:M), b: descr) |> bare_union(atom([nil])) end
      })
      |> Map.fetch!(:M)
      |> to_descr()
    end

    # M = %{atom() => M}
    defp domain_map() do
      recursive(%{M: fn recur -> open_map([{to_domain_keys(atom()), recur.(:M)}]) end})
      |> Map.fetch!(:M)
      |> to_descr()
    end

    # L = non_empty_list(integer(), L) | []
    defp tail_list() do
      recursive(%{
        L: fn recur -> non_empty_list(integer(), recur.(:L)) |> bare_union(empty_list()) end
      })
      |> Map.fetch!(:L)
      |> to_descr()
    end

    # L = non_empty_list(integer(), L) | atom(terminators) — an improper list
    defp improper_list(terminators) do
      recursive(%{
        L: fn recur -> non_empty_list(integer(), recur.(:L)) |> bare_union(atom(terminators)) end
      })
      |> Map.fetch!(:L)
      |> to_descr()
    end

    # E = {E, E} — no base case, so semantically empty
    defp empty_rec() do
      recursive(%{E: fn recur -> tuple([recur.(:E), recur.(:E)]) end})
      |> Map.fetch!(:E)
      |> to_descr()
    end

    # X = {integer(), Y} | nil and Y = {boolean(), X} | nil
    defp mutual_tuples() do
      %{X: nx, Y: ny} =
        recursive(%{
          X: fn recur -> tuple([integer(), recur.(:Y)]) |> bare_union(atom([nil])) end,
          Y: fn recur -> tuple([boolean(), recur.(:X)]) |> bare_union(atom([nil])) end
        })

      {to_descr(nx), to_descr(ny)}
    end

    test "PR example: X = nil | {integer(), X} intersected with itself" do
      node = int_list_node()
      tx = to_descr(node)

      # All argument shapes must terminate: node/node, node/descr, descr/descr
      assert assert_finishes(fn -> equal?(opt_intersection(node, node), tx) end)
      assert assert_finishes(fn -> equal?(opt_intersection(node, tx), tx) end)
      assert assert_finishes(fn -> equal?(opt_intersection(tx, node), tx) end)
      assert assert_finishes(fn -> equal?(opt_intersection(tx, tx), tx) end)

      # Remaining operations on the same type
      assert assert_finishes(fn -> equal?(opt_union(tx, tx), tx) end)
      assert assert_finishes(fn -> empty?(opt_difference(tx, tx)) end)

      neg = assert_finishes(fn -> opt_negation(tx) end)
      assert assert_finishes(fn -> empty?(opt_intersection(tx, neg)) end)
      assert assert_finishes(fn -> equal?(opt_union(tx, neg), term()) end)
      assert assert_finishes(fn -> equal?(opt_negation(neg), tx) end)
    end

    test "fast paths accept nodes" do
      node = int_list_node()
      tx = to_descr(node)

      assert equal?(opt_union(none(), node), tx)
      assert equal?(opt_union(node, none()), tx)
      assert equal?(opt_union(term(), node), term())
      assert equal?(opt_intersection(term(), node), tx)
      assert equal?(opt_intersection(node, term()), tx)
      assert equal?(opt_difference(node, none()), tx)
      assert equal?(opt_difference(node, term()), none())
    end

    test "intersection of alpha-equivalent recursive types" do
      # Same shape built twice: distinct node identities, same semantics.
      # The seen set must recognize repetition by BDD, not by node identity.
      head_list = fn ->
        recursive(%{
          L: fn recur ->
            non_empty_list(recur.(:L), empty_list())
            |> bare_union(non_empty_list(integer(), empty_list()))
          end
        })
        |> Map.fetch!(:L)
        |> to_descr()
      end

      builders = [
        tuple_list: &int_linked_list/0,
        binary_tree: fn -> binary_tree(integer()) end,
        map: &rec_map/0,
        map_domain: &domain_map/0,
        tail_list: &tail_list/0,
        head_list: head_list
      ]

      for {label, builder} <- builders do
        a = builder.()
        b = builder.()

        assert assert_finishes(fn -> equal?(opt_intersection(a, b), a) end),
               "#{label}: intersection of alpha-equivalent copies should equal the type"
      end
    end

    test "intersection of overlapping recursive types" do
      int_list = int_linked_list()
      number_list = number_linked_list()

      # int lists are a subtype of number lists
      assert assert_finishes(fn -> equal?(opt_intersection(int_list, number_list), int_list) end)
      assert assert_finishes(fn -> equal?(opt_intersection(number_list, int_list), int_list) end)

      # int and float lists only share the base case
      float_list = linked_list(float())

      assert assert_finishes(fn ->
               equal?(opt_intersection(int_list, float_list), atom([nil]))
             end)

      # trees
      int_tree = binary_tree(integer())
      number_tree = binary_tree(bare_union(integer(), float()))
      assert assert_finishes(fn -> equal?(opt_intersection(int_tree, number_tree), int_tree) end)

      # mutual recursion crosses two equations before repeating
      {tx, ty} = mutual_tuples()
      assert assert_finishes(fn -> equal?(opt_intersection(tx, tx), tx) end)
      assert assert_finishes(fn -> equal?(opt_intersection(tx, ty), atom([nil])) end)

      # recursive maps where one field is wider
      t_int = rec_map_with(integer())
      t_number = rec_map_with(bare_union(integer(), float()))
      assert assert_finishes(fn -> equal?(opt_intersection(t_int, t_number), t_int) end)

      # an empty recursive type intersected with a non-empty one
      assert assert_finishes(fn -> empty?(opt_intersection(empty_rec(), int_list)) end)
    end

    test "union of recursive types" do
      int_list = int_linked_list()
      number_list = number_linked_list()

      # same descr and alpha-equivalent copies
      assert assert_finishes(fn -> equal?(opt_union(int_list, int_list), int_list) end)
      assert assert_finishes(fn -> equal?(opt_union(int_list, int_linked_list()), int_list) end)

      # subtype absorption
      assert assert_finishes(fn -> equal?(opt_union(int_list, number_list), number_list) end)
      assert assert_finishes(fn -> equal?(opt_union(number_list, int_list), number_list) end)

      # map union with one differing key holding recursive nodes: this is the
      # one_key_difference strategy, which recursively unions the field values
      m1 = rec_map()
      m2 = rec_map()
      assert assert_finishes(fn -> equal?(opt_union(m1, m2), m1) end)

      # one differing non-recursive key next to a shared recursive node
      node = rec_map_node()

      assert assert_finishes(fn ->
               union =
                 opt_union(closed_map(a: node, b: integer()), closed_map(a: node, b: float()))

               equal?(union, closed_map(a: node, b: bare_union(integer(), float())))
             end)

      # tuple union with one differing index holding recursive nodes
      assert assert_finishes(fn ->
               union = opt_union(tuple([integer(), node]), tuple([float(), node]))
               equal?(union, tuple([bare_union(integer(), float()), node]))
             end)

      # empty recursive type is a neutral element
      assert assert_finishes(fn -> equal?(opt_union(empty_rec(), int_list), int_list) end)
    end

    test "difference of recursive types" do
      int_list = int_linked_list()
      number_list = number_linked_list()

      # differences that should be empty
      assert assert_finishes(fn -> empty?(opt_difference(int_list, int_list)) end)
      assert assert_finishes(fn -> empty?(opt_difference(int_list, int_linked_list())) end)
      assert assert_finishes(fn -> empty?(opt_difference(int_list, number_list)) end)

      # a strict difference: number lists that are not int lists
      diff = assert_finishes(fn -> opt_difference(number_list, int_list) end)
      refute assert_finishes(fn -> empty?(diff) end)
      assert assert_finishes(fn -> subtype?(tuple([float(), atom([nil])]), diff) end)

      # recursive maps, alpha-equivalent: one_key_difference recursion
      assert assert_finishes(fn -> empty?(opt_difference(rec_map(), rec_map())) end)

      # improper lists with different terminators: recursion through the tail
      ta = improper_list([:a, :b])
      tb = improper_list([:a])
      diff = assert_finishes(fn -> opt_difference(ta, tb) end)
      assert assert_finishes(fn -> subtype?(atom([:b]), diff) end)
      refute assert_finishes(fn -> empty?(diff) end)
      assert assert_finishes(fn -> empty?(opt_difference(tb, ta)) end)
    end

    test "difference reaching the same recursive pair under different operations" do
      # opt_map_leaf_one_key_difference computes the difference, union or
      # intersection of the same pair of field values depending on the shape
      # of the surrounding BDD. The seen set must key on the operation as well.
      # No closed forms here, so results are checked against the bare ops.
      node_a = rec_map_node()
      node_b = rec_map_node()

      m_at = closed_map(a: node_a, t: atom())
      m_az = closed_map(a: node_a, t: atom([:z]))
      m_bt = closed_map(a: node_b, t: atom())
      m_bz = closed_map(a: node_b, t: atom([:z]))

      # a left BDD with union/negation structure makes the leaf difference
      # request the union of the recursive field values (type: :union)
      for left <- [bare_difference(m_at, m_az), bare_union(m_at, m_az)] do
        assert assert_finishes(fn ->
                 equal?(opt_difference(left, m_bt), bare_difference(left, m_bt))
               end)
      end

      # a negated right side exercises the same leaves under negation
      neg = bare_negation(m_bt)

      assert assert_finishes(fn ->
               equal?(opt_difference(m_at, neg), bare_difference(m_at, neg))
             end)

      assert assert_finishes(fn ->
               equal?(opt_intersection(m_at, neg), bare_intersection(m_at, neg))
             end)

      # a structured right side exercises the intersection context (type: :intersection)
      for right <- [bare_difference(m_bt, m_bz), bare_union(m_bt, m_bz)] do
        assert assert_finishes(fn ->
                 equal?(opt_difference(m_at, right), bare_difference(m_at, right))
               end)
      end

      # double negation through opt operations
      ta = to_descr(node_a)
      assert assert_finishes(fn -> equal?(opt_negation(opt_negation(ta)), ta) end)
    end

    test "operations on dynamic recursive types" do
      int_list = int_linked_list()
      number_list = number_linked_list()

      combos = [
        {dynamic(int_list), int_list},
        {int_list, dynamic(int_list)},
        {dynamic(int_list), dynamic(number_list)},
        {dynamic(int_list), number_list},
        {number_list, dynamic(int_list)}
      ]

      for {left, right} <- combos, op <- [:union, :intersection, :difference] do
        assert assert_finishes(fn ->
                 equal?(opt_op(op, left, right), bare_op(op, left, right))
               end),
               "opt_#{op} disagrees with bare_#{op} on dynamic combination"
      end

      # gradual element inside the recursive definition itself
      build = fn ->
        recursive(%{
          X: fn recur -> tuple([dynamic(integer()), recur.(:X)]) |> bare_union(atom([nil])) end
        })
        |> Map.fetch!(:X)
        |> to_descr()
      end

      tx = build.()
      ty = build.()

      # Gradual parts hidden behind a recursive node are not lifted to the
      # top of the descr (split_dynamic/1 treats nodes as static), so the
      # equal? oracle is imprecise on these types: even bare_intersection of
      # tx with an alpha-equivalent copy is not equal? to tx. We require
      # termination and agreement on concrete witnesses instead.
      witness = tuple([dynamic(integer()), atom([nil])])

      for op <- [:union, :intersection, :difference] do
        assert assert_finishes(fn ->
                 opt = opt_op(op, tx, ty)
                 bare = bare_op(op, tx, ty)

                 subtype?(witness, opt) == subtype?(witness, bare) and
                   empty?(opt) == empty?(bare)
               end),
               "gradual element #{op}: opt and bare disagree on witnesses"
      end
    end

    test "operations on nested mixed-constructor recursion" do
      # map <-> tuple mutual recursion: P = %{items: Q} | nil, Q = {integer(), P} | nil
      build_pq = fn ->
        recursive(%{
          P: fn recur -> closed_map(items: recur.(:Q)) |> bare_union(atom([nil])) end,
          Q: fn recur -> tuple([integer(), recur.(:P)]) |> bare_union(atom([nil])) end
        })
        |> Map.fetch!(:P)
        |> to_descr()
      end

      tp1 = build_pq.()
      tp2 = build_pq.()

      assert assert_finishes(fn -> equal?(opt_intersection(tp1, tp2), tp1) end)
      assert assert_finishes(fn -> equal?(opt_union(tp1, tp2), tp1) end)
      assert assert_finishes(fn -> empty?(opt_difference(tp1, tp2)) end)

      # recursion through a list of maps: E = list(%{next: E} | nil)
      build_e = fn ->
        recursive(%{
          E: fn recur -> list(bare_union(closed_map(next: recur.(:E)), atom([nil]))) end
        })
        |> Map.fetch!(:E)
        |> to_descr()
      end

      te1 = build_e.()
      te2 = build_e.()
      assert assert_finishes(fn -> equal?(opt_intersection(te1, te2), te1) end)
      assert assert_finishes(fn -> equal?(opt_union(te1, te2), te1) end)
      assert assert_finishes(fn -> empty?(opt_difference(te1, te2)) end)

      # json against an alpha-equivalent copy, checked against the bare ops
      build_json = fn ->
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

            scalar |> bare_union(array) |> bare_union(object)
          end
        })
        |> Map.fetch!(:Json)
        |> to_descr()
      end

      json1 = build_json.()
      json2 = build_json.()

      for op <- [:union, :intersection, :difference] do
        assert assert_finishes(fn ->
                 equal?(opt_op(op, json1, json2), bare_op(op, json1, json2))
               end),
               "opt_#{op} disagrees with bare_#{op} on json"
      end
    end

    ## The systematic sweep: every opt operation must agree with its bare
    ## counterpart over a zoo of recursive types. Failures are accumulated
    ## so a regression reports every broken combination at once.

    defp opt_op(:union, left, right), do: opt_union(left, right)
    defp opt_op(:intersection, left, right), do: opt_intersection(left, right)
    defp opt_op(:difference, left, right), do: opt_difference(left, right)

    defp bare_op(:union, left, right), do: bare_union(left, right)
    defp bare_op(:intersection, left, right), do: bare_intersection(left, right)
    defp bare_op(:difference, left, right), do: bare_difference(left, right)

    # Returns a singleton error list if fun does not finish or returns false.
    defp agreement_error(fun) do
      case run_guarded(fun) do
        {:ok, true} -> []
        {:ok, false} -> ["optimized and bare results disagree"]
        {:error, message} -> [message]
      end
    end

    # Covers every structural kind the optimized operations recurse into,
    # plus non-recursive controls.
    defp opt_zoo do
      {mutual, _} = mutual_tuples()

      [
        int_list: int_linked_list(),
        number_list: number_linked_list(),
        int_tree: binary_tree(integer()),
        mutual: mutual,
        rec_map: rec_map(),
        rec_map_int: rec_map_with(integer()),
        map_domain: domain_map(),
        tail_list: tail_list(),
        improper_list: improper_list([:stop, :halt]),
        empty_rec: empty_rec(),
        plain_tuple: tuple([integer(), atom()]),
        plain_scalar: bare_union(atom([nil]), integer())
      ]
    end

    test "systematic: opt operations agree with bare operations over the zoo" do
      # Two zoos, so same-name pairs are alpha-equivalent, not identical
      zoo1 = opt_zoo()
      zoo2 = opt_zoo()

      binary_errors =
        for {name1, t1} <- zoo1,
            {name2, t2} <- zoo2,
            op <- [:union, :intersection, :difference],
            error <- agreement_error(fn -> equal?(opt_op(op, t1, t2), bare_op(op, t1, t2)) end) do
          "#{name1} #{op} #{name2}: #{error}"
        end

      negation_errors =
        for {name, t} <- zoo1,
            error <- agreement_error(fn -> equal?(opt_negation(t), bare_negation(t)) end) do
          "negation of #{name}: #{error}"
        end

      errors = binary_errors ++ negation_errors

      if errors != [] do
        flunk("#{length(errors)} optimized operation(s) failed:\n" <> Enum.join(errors, "\n"))
      end
    end
  end
end
