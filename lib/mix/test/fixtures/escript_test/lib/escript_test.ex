defmodule EscriptTest do
  def start do
    :ok = Application.start(:escript_test)
  end

  def main(["--protocol", protocol]) do
    IO.puts(Protocol.consolidated?(Module.concat([protocol])))
  end

  def main(["--nesting"]) do
    IO.inspect(Application.get_env(:foobar, :nesting, "TEST"))
  end

  def main(["--app-paths"]) do
    elixir_path = Application.app_dir(:elixir) |> String.to_charlist()
    escript_test_path = Application.app_dir(:escript_test) |> String.to_charlist()

    IO.inspect({
      elixir_path != escript_test_path,
      match?({:ok, _}, :erl_prim_loader.list_dir(elixir_path)),
      match?({:ok, _}, :erl_prim_loader.list_dir(escript_test_path))
    })
  end

  def main(["--list-priv", app_name]) do
    # Note: :erl_prim_loader usage with Escript is currently deprecated,
    # but we use it only in tests for convenience

    app = String.to_atom(app_name)
    :erl_prim_loader.list_dir(~c"#{:code.lib_dir(app)}/priv") |> IO.inspect()
  end

  def main(_argv) do
    IO.puts(Application.get_env(:foobar, :value, "TEST"))
  end
end
