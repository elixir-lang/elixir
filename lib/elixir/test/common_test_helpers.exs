defmodule CodeHelpers do
  defp fixture_dir() do
    Path.expand("elixir/fixtures/docs", __DIR__)
  end

  def enter_fixture_dir() do
    Path.wildcard(fixture_dir <> "/*.beam")
    |> Enum.map(&File.rm/1)
    Code.prepend_path(fixture_dir)
    :ok
  end

  def leave_fixture_dir() do
    :code.get_path()
    |> List.delete(fixture_dir)
    |> :code.set_path()
    :ok
  end

  defmacro defbeam(kind \\ :module, name, do_body) do
    mod = case kind do
      :module ->
        quote do
          name = unquote(name); defmodule(name, unquote(do_body))
        end

      :protocol ->
        quote do
          name = unquote(name); defprotocol(name, unquote(do_body))
        end
    end
    quote do
      {:module, _, bin, _} = unquote(mod)
      beam_path = Path.join(unquote(fixture_dir), atom_to_binary(name) <> ".beam")
      File.write!(beam_path, bin)
    end
  end
end
