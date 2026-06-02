# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.InferTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr

  defmacro infer(config, do: block) do
    quote do
      runtime_infer(unquote(config).test, unquote(Macro.escape(block)))
    end
  end

  defp runtime_infer(module, block) do
    {{:module, _, binary, _}, []} =
      Code.eval_quoted(
        quote do
          defmodule unquote(module), do: unquote(block)
        end,
        []
      )

    version = :elixir_erl.checker_version()
    {:ok, {_, [{~c"ExCk", chunk}]}} = :beam_lib.chunks(binary, [~c"ExCk"])
    {^version, data} = :erlang.binary_to_term(chunk)
    for {fun, %{sig: sig}} <- data.exports, into: %{}, do: {fun, sig}
  end

  test "from patterns", config do
    types =
      infer config do
        def fun1(%y{}, %x{}, x = y, x = Point), do: :ok
        def fun2(%x{}, %y{}, x = y, x = Point), do: :ok
        def fun3(%y{}, %x{}, x = y, y = Point), do: :ok
        def fun4(%x{}, %y{}, x = y, y = Point), do: :ok
      end

    args = [
      open_map(__struct__: atom([Point])),
      open_map(__struct__: atom([Point])),
      atom([Point]),
      atom([Point])
    ]

    assert types[{:fun1, 4}] == {:infer, nil, [{args, atom([:ok])}]}
    assert types[{:fun2, 4}] == {:infer, nil, [{args, atom([:ok])}]}
    assert types[{:fun3, 4}] == {:infer, nil, [{args, atom([:ok])}]}
    assert types[{:fun4, 4}] == {:infer, nil, [{args, atom([:ok])}]}
  end

  test "from patterns (with merging)", config do
    types =
      infer config do
        def fun(:ok), do: :one
        def fun("two"), do: :two
        def fun("three"), do: :three
        def fun("four"), do: :four
        def fun(:error), do: :five
      end

    assert types[{:fun, 1}] ==
             {:infer, [opt_union(atom([:ok, :error]), binary())],
              [
                {[atom([:ok])], atom([:one])},
                {[binary()], atom([:two, :three, :four])},
                {[atom([:error])], atom([:five])}
              ]}
  end

  test "from patterns (with overlapping simplifications)", config do
    types =
      infer config do
        # Test that we don't return differences on catch-all
        def foo(%{foo: 123}, {:ok, _}), do: 1
        def foo(%{}, {_, _}), do: 2

        def bar(%{foo: 123}, {:ok, _}), do: 1
        def bar(map, tuple), do: {map, tuple}

        # Notice that we will return a broader domain than the inferred types
        # but because the domain is only used for refining reverse arrows,
        # type checking will still fail if %{foo: not integer()} is given
        def baz(%{foo: var}), do: Integer.to_string(var)
        def baz(%{}), do: :error
      end

    assert {:infer, domain, _} = types[{:foo, 2}]
    assert domain == [open_map(), tuple([term(), term()])]

    assert {:infer, domain, _} = types[{:bar, 2}]
    assert domain == [term(), term()]

    assert {:infer, domain, _} = types[{:baz, 1}]
    assert domain == [open_map()]
  end

  test "from patterns (with grouped return type)", config do
    types =
      infer config do
        def operator?({:comp_op, _, _}), do: true
        def operator?({:comp_op2, _, _}), do: true
        def operator?({:dual_op, _, _}), do: true
        def operator?({:mult_op, _, _}), do: true
        def operator?({:two_op, _, _}), do: true
        def operator?({:concat_op, _, _}), do: true
        def operator?({:ternary_op, _, _}), do: true
        def operator?({:rel_op, _, _}), do: true
        def operator?({:rel_op2, _, _}), do: true
        def operator?({:and_op, _, _}), do: true
        def operator?({:or_op, _, _}), do: true
        def operator?({:match_op, _, _}), do: true
        def operator?({:in_match_op, _, _}), do: true
        def operator?({:stab_op, _, _}), do: true
        def operator?({:pipe_op, _, _}), do: true
        def operator?({:arrow_op, _, _}), do: false
        def operator?(_), do: false
      end

    assert {:infer, [domain], clauses} = types[{:operator?, 1}]
    assert domain |> equal?(term())
    assert length(clauses) == 2
  end

  test "from expressions", config do
    types =
      infer config do
        def fun(x) do
          x.foo + x.bar
        end

        def fun_nested(%{x: x} = data) do
          baz = x.foo + x.bar
          IO.inspect(data)
          baz
        end
      end

    number = opt_union(integer(), float())

    assert types[{:fun, 1}] ==
             {:infer, nil, [{[open_map(foo: number, bar: number)], dynamic(number)}]}

    assert types[{:fun_nested, 1}] ==
             {:infer, nil, [{[open_map(x: open_map(foo: number, bar: number))], dynamic(number)}]}
  end

  test "from Elixir built-in", config do
    types =
      infer config do
        def parse(string), do: Integer.parse(string)
      end

    assert types[{:parse, 1}] ==
             {:infer, nil,
              [{[term()], dynamic(opt_union(atom([:error]), tuple([integer(), binary()])))}]}
  end

  test "from private functions", config do
    types =
      infer config do
        def pub(x), do: priv(x)
        defp priv(:ok), do: :ok
        defp priv(:error), do: :error
      end

    assert types[{:pub, 1}] ==
             {:infer, nil, [{[atom([:ok, :error])], dynamic(atom([:ok, :error]))}]}

    assert types[{:priv, 1}] == nil
  end

  test "from super functions", config do
    types =
      infer config do
        def pub(:ok), do: :ok
        def pub(:error), do: :error
        defoverridable pub: 1
        def pub(x), do: {:ok, super(x)}
      end

    assert types[{:pub, 1}] ==
             {:infer, nil,
              [{[atom([:ok, :error])], dynamic(tuple([atom([:ok]), atom([:ok, :error])]))}]}
  end

  test "from cyclic calls", config do
    types =
      infer config do
        def pub(x), do: pub(x)
      end

    assert types[{:pub, 1}] == {:infer, nil, [{[term()], dynamic()}]}
  end

  test "from structs", config do
    types =
      infer config do
        defstruct [:x, :y, :z]

        def struct_create_with_atom_keys(x) do
          infer(y = %__MODULE__{x: x})
          {x, y}
        end

        def map_create_with_atom_keys(x) do
          infer(%{__struct__: __MODULE__, x: x, y: nil, z: nil})
          x
        end

        def map_update_with_atom_keys(x) do
          infer(%{x | y: nil})
          x
        end

        def map_update_with_unknown_keys(x, key) do
          infer(%{x | key => 123})
          x
        end

        defp infer(%__MODULE__{x: <<_::binary>>, y: nil}) do
          :ok
        end
      end

    module = config.test

    assert {:infer, _, [{_, return}]} = types[{:struct_create_with_atom_keys, 1}]

    assert return ==
             dynamic(
               tuple([
                 binary(),
                 closed_map(
                   __struct__: atom([module]),
                   x: binary(),
                   y: atom([nil]),
                   z: atom([nil])
                 )
               ])
             )

    assert {:infer, _, [{_, return}]} = types[{:map_create_with_atom_keys, 1}]
    assert return == dynamic(binary())

    assert {:infer, _, [{_, return}]} = types[{:map_update_with_atom_keys, 1}]

    assert return ==
             dynamic(
               closed_map(
                 __struct__: atom([module]),
                 x: binary(),
                 y: atom([nil]),
                 z: term()
               )
             )

    assert {:infer, _, [{_, return}]} = types[{:map_update_with_unknown_keys, 2}]

    assert return ==
             dynamic(
               closed_map(
                 __struct__: atom([module]),
                 x: binary(),
                 y: atom([nil]),
                 z: term()
               )
             )
  end

  test "from captures", config do
    types =
      infer config do
        def captured, do: &to_capture/1
        defp to_capture(<<"ok">>), do: :ok
        defp to_capture(<<"error">>), do: :error
        defp to_capture([_ | _]), do: :list
      end

    assert {:infer, _, [{_, return}]} = types[{:captured, 0}]

    assert equal?(
             return,
             fun_from_non_overlapping_clauses([
               {[binary()], dynamic(atom([:ok, :error]))},
               {[non_empty_list(term(), term())], dynamic(atom([:list]))}
             ])
           )
  end

  test "from defaults", config do
    types =
      infer config do
        # We should only stick with the clause we use
        def filtered_ok(x \\ :ok, arg)
        def filtered_ok(:ok, arg) when is_binary(arg), do: {:ok_binary, arg}
        def filtered_ok(:ok, arg) when is_atom(arg), do: {:ok_atom, arg}
        def filtered_ok(:error, arg), do: {:error, arg}

        # Without polymorphic types, we still return arg as term
        def ok_or_error(x, arg \\ 0)
        def ok_or_error(:ok, arg), do: {:ok, arg}
        def ok_or_error(:error, arg), do: {:error, arg}
      end

    assert {:infer, _, clauses} = types[{:filtered_ok, 1}]

    assert clauses == [
             {[binary()], dynamic(tuple([atom([:ok_binary]), binary()]))},
             {[atom()], dynamic(tuple([atom([:ok_atom]), atom()]))}
           ]

    assert {:infer, _, clauses} = types[{:ok_or_error, 1}]

    assert clauses == [
             {[atom([:ok])], dynamic(tuple([atom([:ok]), term()]))},
             {[atom([:error])], dynamic(tuple([atom([:error]), term()]))}
           ]
  end

  test "from defguard (regression with large code generation)", config do
    # As long as it type checks in time, we are fine,
    # but it should infer Macro.t in the future.
    infer config do
      defguard is_erlang_app(app) when app in ~w(
        inets ftp os_mon parsetools mnesia eldap eunit observer
        dialyzer runtime_tools edoc diameter wx debugger ssh et
        sasl ssl asn1 snmp erts tools stdlib reltool kernel crypto
        tftp erl_interface syntax_tools megaco public_key
        common_test xmerl compiler jinterface
      )
    end
  end

  test "from defaults (regression with multiple clauses)", config do
    types =
      infer config do
        def entries(tree_node, opts \\ [keep_text: true])

        def entries({_, _, subentries}, keep_text: false) do
          Enum.filter(subentries, &is_tuple/1)
        end

        def entries({_, _, subentries}, keep_text: _) do
          subentries
        end

        def entries({_, _, _} = tree_node, opts) do
          opts = Keyword.validate!(opts, keep_text: true)
          entries(tree_node, keep_text: opts[:keep_text])
        end

        def entries(_tree_node, _opts), do: nil
      end

    {:infer, _, signature} = types[{:entries, 1}]

    assert signature == [
             {[tuple([term(), term(), term()])], dynamic()},
             {[opt_negation(tuple([term(), term(), term()]))], atom([nil])}
           ]
  end
end
