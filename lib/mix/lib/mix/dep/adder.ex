defmodule Mix.Dep.Adder do
  def add(app, opts, mix_exs) do
    lines = String.split(mix_exs, "\n")

    check_for_app!(lines, app)

    dep_opts = get_version_str(opts, app) <> get_only_string(opts) <> get_runtime_string(opts)
    new_line = "{:#{app}, #{dep_opts}}"

    new_lines = add_dep_line(lines, [], new_line)

    if lines == new_lines, do: Mix.raise("Unable to identify deps function in mix.exs")

    Mix.shell().info([:green, "Added #{app} to mix.exs: #{new_line}"])

    Enum.join(new_lines, "\n")
  end

  defp get_runtime_string(opts) do
    if Keyword.has_key?(opts, :runtime) and !opts[:runtime] do
      ", runtime: false"
    else
      ""
    end
  end

  defp get_only_string(opts) do
    case opts[:only] do
      [] -> ""
      [env] -> ", only: :#{env}"
      envs -> ", only: [" <> (envs |> Enum.map(&(":" <> &1)) |> Enum.join(", ")) <> "]"
    end
  end

  defp get_version_str(opts, app) do
    case opts[:path] do
      nil ->
        case opts[:version] || latest_hex_version(app) do
          "~> " <> _ = version -> ~s|"#{version}"|
          ">= " <> _ = version -> ~s|"#{version}"|
          "0.0.0" -> ~s|">= 0.0.0"|
          version -> ~s|"~> #{version}"|
        end

      path ->
        ~s|path: "#{path}"|
    end
  end

  defp check_for_app!(lines, app) do
    lines
    |> Enum.find(fn line ->
      line
      |> String.trim()
      |> String.starts_with?("{:" <> app)
    end)
    |> case do
      nil ->
        :ok

      found ->
        Mix.raise("#{app} already exists in mix.exs as #{String.trim(found)}")
    end
  end

  defp latest_hex_version(_app) do
    # TODO: Get latest version from Hex
    "0.0.0"
  end

  defp add_dep_line([line1, line2 | rest], acc, dep_line) do
    cond do
      String.trim(line1) == "defp deps do" && String.trim(line2) == "[" ->
        dep_line =
          if additional_deps?(rest) do
            dep_line <> ","
          else
            dep_line
          end

        Enum.reverse(acc) ++ [line1, line2, indent(dep_line, line2) | rest]

      String.trim(line1) == "defp deps do" && String.trim(line2) == "[]" ->
        Enum.reverse(acc) ++
          [line1, indent("[", line1), indent(dep_line, line2), indent("]", line1) | rest]

      true ->
        add_dep_line([line2 | rest], [line1 | acc], dep_line)
    end
  end

  defp indent(line, parent_line) do
    String.duplicate(" ", indented(parent_line) + 2) <> line
  end

  defp indented(line, count \\ 0)
  defp indented(" " <> rest, count), do: indented(rest, count + 1)
  defp indented(_, count), do: count

  defp additional_deps?([head | tail]) do
    case String.trim(head) do
      "{:" <> _ -> true
      "]" -> false
      _ -> additional_deps?(tail)
    end
  end
end
