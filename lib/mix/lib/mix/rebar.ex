defmodule Mix.Rebar do
  @moduledoc false

  @doc """
  Returns the path supposed to host the local copy of `rebar`.
  """
  def local_rebar_path(manager) do
    Path.join(Mix.Utils.mix_home(), Atom.to_string(manager))
  end

  @doc """
  Returns the path to the global copy of `rebar`, defined by the
  environment variables `MIX_REBAR` or `MIX_REBAR3`.
  """
  def global_rebar_cmd(manager) do
    env = manager_to_env(manager)

    if cmd = System.get_env(env) do
      wrap_cmd(cmd)
    end
  end

  @doc """
  Returns the path to the local copy of `rebar`, if one exists.
  """
  def local_rebar_cmd(manager) do
    cmd = local_rebar_path(manager)

    if File.regular?(cmd) do
      wrap_cmd(cmd)
    end
  end

  @doc """
  Returns the path to the available `rebar` command.
  """
  def rebar_cmd(manager) do
    global_rebar_cmd(manager) || local_rebar_cmd(manager)
  end

  @doc """
  Loads `rebar.config` and evaluates `rebar.config.script` if it
  exists in the given directory.
  """
  def load_config(dir) do
    config_path = Path.join(dir, "rebar.config")
    script_path = Path.join(dir, "rebar.config.script")

    config =
      case :file.consult(config_path) do
        {:ok, config} ->
          config

        {:error, :enoent} ->
          []

        {:error, error} ->
          reason = :file.format_error(error)
          Mix.raise("Error consulting Rebar config #{inspect(config_path)}: #{reason}")
      end

    if File.exists?(script_path) do
      eval_script(script_path, config)
    else
      config
    end
  end

  @doc """
  Serializes a Rebar config to a term file.
  """
  def serialize_config(config) do
    Enum.map(config, &[:io_lib.print(&1) | ".\n"])
  end

  @doc """
  Updates Rebar configuration to be more suitable for dependencies.

  Drops `warnings_as_errors` from `erl_opts`.
  """
  def dependency_config(config) do
    Enum.map(config, fn
      {:erl_opts, opts} ->
        {:erl_opts, List.delete(opts, :warnings_as_errors)}

      other ->
        other
    end)
  end

  @doc """
  Parses the dependencies in given `rebar.config` to Mix's dependency format.
  """
  def deps(config) do
    # We don't have to handle Rebar3 profiles because dependencies
    # are always in the default profile which cannot be customized
    if deps = config[:deps] do
      Enum.map(deps, &parse_dep/1)
    else
      []
    end
  end

  @doc """
  Runs `fun` for the given config and for each `sub_dirs` in the
  given Rebar config.

  `sub_dirs` is only supported in Rebar 2. In Rebar 3, the equivalent
  to umbrella apps cannot be used as dependencies, so we don't need
  to worry about such cases in Mix.
  """
  def recur(config, fun) do
    subs =
      (config[:sub_dirs] || [])
      |> Enum.flat_map(&Path.wildcard(&1))
      |> Enum.filter(&File.dir?(&1))
      |> Enum.flat_map(&recur(load_config(&1), fun))

    [fun.(config) | subs]
  end

  # Translate a Rebar dependency declaration to a Mix declaration
  # From http://www.rebar3.org/docs/dependencies#section-declaring-dependencies
  defp parse_dep(app) when is_atom(app) do
    {app, ">= 0.0.0", override: true}
  end

  defp parse_dep({app, req}) when is_list(req) do
    {app, List.to_string(req), override: true}
  end

  defp parse_dep({app, source}) when is_tuple(source) do
    parse_dep({app, nil, source, []})
  end

  defp parse_dep({app, req, source}) do
    parse_dep({app, req, source, []})
  end

  defp parse_dep({app, req, source, opts}) do
    source = parse_source(source)
    compile = if :proplists.get_value(:raw, opts, false), do: [compile: false], else: []
    {app, compile_req(req), [override: true] ++ source ++ compile}
  end

  defp parse_source({:pkg, pkg}) do
    [hex: pkg]
  end

  defp parse_source(source) do
    [scm, url | source] = Tuple.to_list(source)

    ref =
      case source do
        ["" | _] -> [branch: "HEAD"]
        [{:branch, branch} | _] -> [branch: to_string(branch)]
        [{:tag, tag} | _] -> [tag: to_string(tag)]
        [{:ref, ref} | _] -> [ref: to_string(ref)]
        [ref | _] -> [ref: to_string(ref)]
        _ -> []
      end

    [{scm, to_string(url)}] ++ ref
  end

  defp compile_req(nil) do
    ">= 0.0.0"
  end

  defp compile_req(req) do
    req = List.to_string(req)

    case Version.parse_requirement(req) do
      {:ok, _} ->
        req

      :error ->
        case Regex.compile(req) do
          {:ok, re} ->
            re

          {:error, reason} ->
            Mix.raise("Unable to compile version regular expression: #{inspect(req)}, #{reason}")
        end
    end
  end

  defp manager_to_env(:rebar), do: "MIX_REBAR"
  defp manager_to_env(:rebar3), do: "MIX_REBAR3"

  defp eval_script(script_path, config) do
    script = String.to_charlist(Path.basename(script_path))

    result =
      File.cd!(Path.dirname(script_path), fn ->
        :file.script(script, eval_binds(CONFIG: config, SCRIPT: script))
      end)

    case result do
      {:ok, config} ->
        config

      {:error, error} ->
        reason = :file.format_error(error)
        Mix.shell().error("Error evaluating Rebar config script #{script_path}:#{reason}")

        Mix.shell().error(
          "Any dependencies defined in the script won't be available " <>
            "unless you add them to your Mix project"
        )

        config
    end
  end

  defp eval_binds(binds) do
    Enum.reduce(binds, :erl_eval.new_bindings(), fn {k, v}, binds ->
      :erl_eval.add_binding(k, v, binds)
    end)
  end

  defp wrap_cmd(rebar) do
    cond do
      not match?({:win32, _}, :os.type()) ->
        rebar

      String.ends_with?(rebar, ".cmd") ->
        "\"#{String.replace(rebar, "/", "\\")}\""

      true ->
        "escript.exe \"#{rebar}\""
    end
  end

  @doc """
  Applies the given overrides for app config.
  """
  def apply_overrides(app, config, overrides) do
    # Inefficient. We want the order we get here though.
    config =
      Enum.reduce(overrides, config, fn
        {:override, overrides}, config ->
          Enum.reduce(overrides, config, fn {key, value}, config ->
            Keyword.put(config, key, value)
          end)

        _, config ->
          config
      end)

    config =
      Enum.reduce(overrides, config, fn
        {:override, ^app, overrides}, config ->
          Enum.reduce(overrides, config, fn {key, value}, config ->
            Keyword.put(config, key, value)
          end)

        _, config ->
          config
      end)

    config =
      Enum.reduce(overrides, config, fn
        {:add, ^app, overrides}, config ->
          Enum.reduce(overrides, config, fn {key, value}, config ->
            old_value = Keyword.get(config, key, [])
            Keyword.put(config, key, value ++ old_value)
          end)

        _, config ->
          config
      end)

    Keyword.update(config, :overrides, overrides, &(overrides ++ &1))
  end
end
