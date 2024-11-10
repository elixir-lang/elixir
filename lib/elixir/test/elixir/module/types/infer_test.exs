Code.require_file("type_helper.exs", __DIR__)

defmodule Module.Types.InferTest do
  use ExUnit.Case, async: true

  import Module.Types.Descr

  defmacro infer(config, do: block) do
    quote do
      defmodule unquote(config).test do
        unquote(block)
      end
      |> runtime_infer()
    end
  end

  defp runtime_infer({:module, module, binary, _result}) do
    {:ok, {_, [debug_info: chunk]}} = :beam_lib.chunks(binary, [:debug_info])
    {:debug_info_v1, backend, data} = chunk
    {:ok, %{signatures: signatures}} = backend.debug_info(:elixir_v1, module, data, [])
    signatures
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

    assert types[{:fun1, 4}] == {:infer, [{args, atom([:ok])}]}
    assert types[{:fun2, 4}] == {:infer, [{args, atom([:ok])}]}
    assert types[{:fun3, 4}] == {:infer, [{args, atom([:ok])}]}
    assert types[{:fun4, 4}] == {:infer, [{args, atom([:ok])}]}
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
             {:infer,
              [
                {[dynamic(atom([:ok]))], atom([:one])},
                {[dynamic(binary())], atom([:two, :three, :four])},
                {[dynamic(atom([:error]))], atom([:five])}
              ]}
  end

  test "infers return types from private functions", config do
    types =
      infer config do
        def pub(x), do: priv(x)
        defp priv(:ok), do: :ok
        defp priv(:error), do: :error
      end

    assert types[{:pub, 1}] == {:infer, [{[dynamic()], dynamic(atom([:ok, :error]))}]}
    assert types[{:priv, 1}] == nil
  end

  test "infers return types even with loops", config do
    types =
      infer config do
        def pub(x), do: pub(x)
      end

    assert types[{:pub, 1}] == {:infer, [{[dynamic()], dynamic()}]}
  end
end
