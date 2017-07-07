defmodule IEx.Pry do
  @moduledoc false

  @doc """
  Callback for `IEx.pry/1`.

  You can invoke this function directly when you are not able to invoke
  `IEx.pry/1` as a macro. This function expects the binding (from
  `Kernel.binding/0`) and the environment (from `__ENV__/0`).
  """
  def pry(binding, env) do
    opts = [binding: binding, dot_iex_path: "", env: env, prefix: "pry"]
    meta = "#{inspect self()} at #{Path.relative_to_cwd(env.file)}:#{env.line}"
    desc =
      case whereami(env.file, env.line, 2) do
        {:ok, lines} -> [?\n, ?\n, lines]
        :error -> ""
      end

    res = IEx.Server.take_over("Request to pry #{meta}#{desc}", opts)

    # We cannot use colors because IEx may be off.
    case res do
      {:error, :no_iex} ->
        extra =
          case :os.type do
            {:win32, _} -> " If you are Windows, you may need to start IEx with the --werl flag."
            _           -> ""
          end
        IO.puts :stdio, "Cannot pry #{meta}. Is an IEx shell running?" <> extra
      _ ->
        :ok
    end

    res
  end

  @doc """
  Formats the location for `whereami/3` prying.

  It receives the `file`, `line` and the snippet `radius` and
  returns `{:ok, lines}`, where lines is a list of chardata
  containing each formatted line, or `:error`.

  The actual line is especially formatted in bold.
  """
  def whereami(file, line, radius) do
    with true <- File.regular?(file),
         [_ | _] = lines <- whereami_lines(file, line, radius) do
      {:ok, lines}
    else
      _ -> :error
    end
  end

  defp whereami_lines(file, line, radius) do
    min = max(line - radius - 1, 0)
    max = line + radius - 1

    file
    |> File.stream!
    |> Enum.slice(min..max)
    |> Enum.with_index(min + 1)
    |> Enum.map(&whereami_format_line(&1, line))
  end

  defp whereami_format_line({line_text, line_number}, line) do
    gutter = String.pad_leading(Integer.to_string(line_number), 5, " ")
    if line_number == line do
      IO.ANSI.format_fragment([:bright, gutter, ": ", line_text, :normal])
    else
      [gutter, ": ", line_text]
    end
  end
end
