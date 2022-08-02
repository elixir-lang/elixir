defmodule IEx.Autocomplete.Path do
  @moduledoc false

  @behaviour IEx.Autocomplete.Behaviour

  def expandable_fragment(code), do: path_fragment(code)

  def expand(code, _shell), do: expand_path(code)

  defp path_fragment(expr), do: path_fragment(expr, [])
  defp path_fragment([], _acc), do: []
  defp path_fragment([?{, ?# | _rest], _acc), do: []
  defp path_fragment([?", ?\\ | t], acc), do: path_fragment(t, [?\\, ?" | acc])

  defp path_fragment([?/, ?:, x, ?" | _], acc) when x in ?a..?z or x in ?A..?Z,
    do: [x, ?:, ?/ | acc]

  defp path_fragment([?/, ?., ?" | _], acc), do: [?., ?/ | acc]
  defp path_fragment([?/, ?" | _], acc), do: [?/ | acc]
  defp path_fragment([?" | _], _acc), do: []
  defp path_fragment([h | t], acc), do: path_fragment(t, [h | acc])

  defp expand_path(path) do
    path
    |> List.to_string()
    |> ls_prefix()
    |> Enum.map(fn path ->
      %{
        kind: if(File.dir?(path), do: :dir, else: :file),
        name: Path.basename(path)
      }
    end)
    |> IEx.Autocomplete.format_expansion(path_hint(path))
  end

  defp path_hint(path) do
    if List.last(path) in [?/, ?\\] do
      ""
    else
      Path.basename(path)
    end
  end

  defp prefix_from_dir(".", <<c, _::binary>>) when c != ?., do: ""
  defp prefix_from_dir(dir, _fragment), do: dir

  defp ls_prefix(path) do
    dir = Path.dirname(path)
    prefix = prefix_from_dir(dir, path)

    case File.ls(dir) do
      {:ok, list} ->
        list
        |> Enum.map(&Path.join(prefix, &1))
        |> Enum.filter(&String.starts_with?(&1, path))

      _ ->
        []
    end
  end
end
