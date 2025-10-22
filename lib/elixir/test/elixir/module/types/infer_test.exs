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

  test "infer types from patterns", config do
    types =
      infer config do
        def fun1(%y{}, %x{}, x = y, x = Point), do: :ok
        def fun2(%x{}, %y{}, x = y, x = Point), do: :ok
        def fun3(%y{}, %x{}, x = y, y = Point), do: :ok
        def fun4(%x{}, %y{}, x = y, y = Point), do: :ok
      end

    args = [
      dynamic(open_map(__struct__: atom([Point]))),
      dynamic(open_map(__struct__: atom([Point]))),
      dynamic(atom([Point])),
      dynamic(atom([Point]))
    ]

    assert types[{:fun1, 4}] == {:infer, nil, [{args, atom([:ok])}]}
    assert types[{:fun2, 4}] == {:infer, nil, [{args, atom([:ok])}]}
    assert types[{:fun3, 4}] == {:infer, nil, [{args, atom([:ok])}]}
    assert types[{:fun4, 4}] == {:infer, nil, [{args, atom([:ok])}]}
  end

  test "infer with Elixir built-in", config do
    types =
      infer config do
        def parse(string), do: Integer.parse(string)
      end

    assert types[{:parse, 1}] ==
             {:infer, nil,
              [{[dynamic()], dynamic(union(atom([:error]), tuple([integer(), term()])))}]}
  end

  test "merges patterns", config do
    types =
      infer config do
        def fun(:ok), do: :one
        def fun("two"), do: :two
        def fun("three"), do: :three
        def fun("four"), do: :four
        def fun(:error), do: :five
      end

    assert types[{:fun, 1}] ==
             {:infer, [dynamic(union(atom([:ok, :error]), binary()))],
              [
                {[dynamic(atom([:ok]))], atom([:one])},
                {[dynamic(binary())], atom([:two, :three, :four])},
                {[dynamic(atom([:error]))], atom([:five])}
              ]}
  end

  test "infers return types even with loops", config do
    types =
      infer config do
        def pub(x), do: pub(x)
      end

    assert types[{:pub, 1}] == {:infer, nil, [{[dynamic()], dynamic()}]}
  end
end
