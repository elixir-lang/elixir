# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.TypespecTest.SomeStruct do
  defstruct [:name, :age]
end

defmodule Module.Types.TypespecTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr
  alias Module.Types.Typespec

  defp to_descr(ast, defined \\ %{}) do
    Typespec.to_descr(ast, %{module: __MODULE__, defined: defined})
  end

  describe "built-in types" do
    test "atomic types" do
      assert to_descr(quote(do: integer())) == {:ok, integer()}
      assert to_descr(quote(do: float())) == {:ok, float()}
      assert to_descr(quote(do: atom())) == {:ok, atom()}
      assert to_descr(quote(do: boolean())) == {:ok, boolean()}
      assert to_descr(quote(do: pid())) == {:ok, pid()}
      assert to_descr(quote(do: port())) == {:ok, port()}
      assert to_descr(quote(do: reference())) == {:ok, reference()}
      assert to_descr(quote(do: binary())) == {:ok, binary()}
      assert to_descr(quote(do: bitstring())) == {:ok, bitstring()}
    end

    test "top and bottom" do
      assert to_descr(quote(do: any())) == {:ok, term()}
      assert to_descr(quote(do: term())) == {:ok, term()}
      assert to_descr(quote(do: none())) == {:ok, none()}
      assert to_descr(quote(do: no_return())) == {:ok, none()}
    end

    test "collections" do
      assert to_descr(quote(do: map())) == {:ok, open_map()}
      assert to_descr(quote(do: tuple())) == {:ok, tuple()}
      assert to_descr(quote(do: list(integer()))) == {:ok, list(integer())}
      assert to_descr(quote(do: non_empty_list(integer()))) == {:ok, non_empty_list(integer())}
    end
  end

  describe "literals" do
    test "atoms" do
      assert to_descr(quote(do: :foo)) == {:ok, atom([:foo])}
      assert to_descr(quote(do: nil)) == {:ok, atom([nil])}
      assert to_descr(quote(do: true)) == {:ok, atom([true])}
    end

    test "integers" do
      assert to_descr(quote(do: 42)) == {:ok, integer()}
    end

    test "empty list" do
      assert to_descr(quote(do: [])) == {:ok, empty_list()}
    end

    test "tuple literal" do
      assert to_descr(quote(do: {integer(), atom()})) ==
               {:ok, tuple([integer(), atom()])}
    end

    test "empty map literal" do
      assert to_descr(quote(do: %{})) == {:ok, empty_map()}
    end
  end

  describe "structs" do
    test "%__MODULE__{} expands to closed_map with struct tag" do
      assert {:ok, descr} =
               to_descr(
                 quote(
                   do: %Module.Types.TypespecTest.SomeStruct{name: binary(), age: integer()}
                 )
               )

      assert equal?(
               descr,
               closed_map([
                 {:__struct__, atom([Module.Types.TypespecTest.SomeStruct])},
                 {:name, binary()},
                 {:age, integer()}
               ])
             )
    end
  end

  describe "compositions" do
    test "union" do
      assert {:ok, descr} = to_descr(quote(do: integer() | atom()))
      assert equal?(descr, union(integer(), atom()))
    end

    test "three-way union" do
      assert {:ok, descr} = to_descr(quote(do: integer() | atom() | binary()))
      assert equal?(descr, union(integer(), union(atom(), binary())))
    end
  end

  describe "local references" do
    test "resolves a local @type alias" do
      defined = %{{:id, 0} => {:type, integer()}}
      assert to_descr(quote(do: id()), defined) == {:ok, integer()}
    end

    test "resolves transitively through stored aliases" do
      defined = %{
        {:id, 0} => {:type, integer()},
        {:maybe_id, 0} => {:type, union(integer(), atom([nil]))}
      }

      assert {:ok, descr} = to_descr(quote(do: maybe_id()), defined)
      assert equal?(descr, union(integer(), atom([nil])))
    end
  end

  describe "function types" do
    test "single-arg function" do
      assert {:ok, descr} = to_descr(quote(do: (integer() -> integer())))
      assert equal?(descr, fun([integer()], integer()))
    end

    test "multi-arg function" do
      assert {:ok, descr} = to_descr(quote(do: (integer(), atom() -> :ok)))
      assert equal?(descr, fun([integer(), atom()], atom([:ok])))
    end

    test "zero-arg function" do
      assert {:ok, descr} = to_descr(quote(do: (-> :ok)))
      assert equal?(descr, fun([], atom([:ok])))
    end

    test "variadic ... -> result degrades to top function" do
      assert {:ok, descr} = to_descr(quote(do: (... -> integer())))
      assert equal?(descr, fun())
    end
  end

  describe "errors" do
    test "cycle is reported" do
      defined = %{{:a, 0} => :pending}
      assert {:error, {:cycle, :a, 0}} = to_descr(quote(do: a()), defined)
    end

    test "parametric arity > 0 reference returns parametric_unsupported" do
      defined = %{{:t, 1} => {:type, term()}}
      assert {:error, {:parametric_unsupported, :t, 1}} = to_descr(quote(do: t(integer())), defined)
    end
  end

  describe "remote type references" do
    test "resolves to the stored Descr via ExCk" do
      # `String.t :: binary()` is a stable stdlib type that ships with
      # an ExCk `:types` entry produced by this same converter.
      assert {:ok, descr} = to_descr(quote(do: String.t()))
      assert equal?(descr, binary())
    end

    test "unknown name in known module degrades to dynamic()" do
      assert {:ok, descr} = to_descr(quote(do: String.nonexistent_type()))
      assert equal?(descr, dynamic())
    end

    test "Erlang module reference degrades to dynamic()" do
      # Erlang modules don't ship an ExCk chunk; we degrade quietly.
      assert {:ok, descr} = to_descr(quote(do: :lists.boolean()))
      assert equal?(descr, dynamic())
    end

    test "parametric remote ref degrades to dynamic() (graceful)" do
      # The offending parametric typespec lives in another module; we
      # can't throw a useful error from here, so degrade quietly.
      assert {:ok, descr} = to_descr(quote(do: Enumerable.t(integer())))
      assert equal?(descr, dynamic())
    end
  end
end
