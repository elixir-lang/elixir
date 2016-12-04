defmodule Mix.Rebar do
  @moduledoc false

  @doc """
  Returns the path supposed to host the local copy of `rebar`.
  """
  def local_rebar_path(manager) do
    Path.join(Mix.Utils.mix_home, Atom.to_string(manager))
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

    config = case :file.consult(config_path) do
      {:ok, config} ->
        config
      {:error, :enoent} ->
        []
      {:error, error} ->
        reason = :file.format_error(error)
        Mix.raise "Error consulting Rebar config #{inspect config_path}: #{reason}"
    end

    if File.exists?(script_path) do
      eval_script(script_path, config)
    else
      config
    end
  end

  @doc """
  Merges a rebar3 parent config with a child config.
  """
  # From https://github.com/rebar/rebar3/blob/b1da2ec0674df89599564252734bd4d794436425/src/rebar_opts.erl#L103
  def merge_config(old, new) do
    Keyword.merge(old, new, fn
      :deps, old, _new               -> old
      {:deps, _}, _old, new          -> new
      :plugins, _old, new            -> new
      {:plugins, _}, _old, new       -> new
      :profiles, old, new            -> merge_config(old, new)
      :mib_first_files, value, value -> value
      :mib_first_files, old, new     -> old ++ new
      :relx, old, new                -> tuple_merge(new, old)
      _key, old, new when is_list(new) ->
        case :io_lib.printable_list(new) do
          true when new == [] ->
            if :io_lib.printable_list(old), do: new, else: old
          true ->
            new
          false ->
            tuple_merge(old, new)
        end
      _key, _old, new -> new
    end)
  end

  # From https://github.com/rebar/rebar3/blob/b1da2ec0674df89599564252734bd4d794436425/src/rebar_utils.erl#L282
  defp tuple_merge(old, new),
    do: do_tuple_merge(tuple_sort(old), tuple_sort(new))

  defp do_tuple_merge(old, []),
    do: old
  defp do_tuple_merge(olds, [new | news]),
    do: do_tuple_umerge_dedup(umerge(:new, olds, [], news, new), [])

  defp umerge(_, [], [], acc, current),
    do: [current | acc]
  defp umerge(:new, [], news, acc, current),
    do: Enum.reverse(news, [current | acc])
  defp umerge(:old, olds, [], acc, current),
    do: Enum.reverse(olds, [current | acc])
  defp umerge(:new, [old | olds], news, acc, current) do
    {dir, merged, new_current} = compare({:new, current}, {:old, old})
    umerge(dir, olds, news, [merged | acc], new_current)
  end
  defp umerge(:old, olds, [new | news], acc, current) do
    {dir, merged, new_current} = compare({:new, new}, {:old, current})
    umerge(dir, olds, news, [merged | acc], new_current)
  end

  defp compare({priority, a}, {secondary, b}) when is_tuple(a) and is_tuple(b) do
    ka = elem(a, 0)
    kb = elem(b, 0)
    cond do
      ka == kb -> {secondary, a, b}
      ka  < kb -> {secondary, a, b}
      ka  > kb -> {priority, b, a}
    end
  end
  defp compare({priority, a}, {secondary, b}) when not is_tuple(a) and not is_tuple(b) do
    cond do
      a == b -> {secondary, a, b}
      a  < b -> {secondary, a, b}
      a  > b -> {priority, b, a}
    end
  end
  defp compare({priority, a}, {secondary, b}) when is_tuple(a) and not is_tuple(b) do
    ka = elem(a, 0)
    cond do
      ka == b -> {secondary, a, b}
      ka  < b -> {secondary, a, b}
      ka  > b -> {priority, b, a}
    end
  end
  defp compare({priority, a}, {secondary, b}) when not is_tuple(a) and is_tuple(b) do
    kb = elem(b, 0)
    cond do
      a == kb -> {secondary, a, b}
      a  < kb -> {secondary, a, b}
      a  > kb -> {priority, b, a}
    end
  end

  defp do_tuple_umerge_dedup([], acc), do: acc
  defp do_tuple_umerge_dedup([h | t], acc) do
    if h in t do
      do_tuple_umerge_dedup(t, acc)
    else
      do_tuple_umerge_dedup(t, [h | acc])
    end
  end

  defp tuple_sort(list) do
    Enum.sort(list, fn
      a, b when is_tuple(a) and is_tuple(b) -> elem(a, 0) <= elem(b, 0)
      a, b when is_tuple(a) -> elem(a, 0) <= b
      a, b when is_tuple(b) -> a <= elem(b, 0)
      a, b -> a <= b
    end)
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
        {:erl_opts, Enum.reject(opts, &(&1 == :warnings_as_errors))}
      other ->
        other
    end)
  end

  @doc """
  Parses the dependencies in given `rebar.config` to Mix's dependency format.
  """
  def deps(app, config, overrides) do
    # We don't have to handle rebar3 profiles because dependencies
    # are always in the default profile which cannot be customized
    config = apply_overrides(app, config, overrides)
    if deps = config[:deps] do
      Enum.map(deps, &parse_dep/1)
    else
      []
    end
  end

  @doc """
  Runs `fun` for the given config and for each `sub_dirs` in the
  given Rebar config.
  """
  def recur(config, fun) when is_binary(config) do
    recur(load_config(config), fun)
  end

  def recur(config, fun) do
    subs =
      (config[:sub_dirs] || [])
      |> Enum.map(&Path.wildcard(&1))
      |> Enum.concat
      |> Enum.filter(&File.dir?(&1))
      |> Enum.map(&recur(&1, fun))
      |> Enum.concat

    [fun.(config) | subs]
  end

  defp parse_dep(app) when is_atom(app) do
    {app, ">= 0.0.0"}
  end

  defp parse_dep({app, req}) when is_list(req) do
    {app, List.to_string(req)}
  end

  defp parse_dep({app, req, {:pkg, package}}) when is_list(req) do
    {app, List.to_string(req), hex: package}
  end

  defp parse_dep({app, source}) when is_tuple(source) do
    parse_dep({app, nil, source, []})
  end

  defp parse_dep({app, req, source}) do
    parse_dep({app, req, source, []})
  end

  defp parse_dep({app, req, source, opts}) do
    [scm, url | source] = Tuple.to_list(source)

    ref =
      case source do
        ["" | _]                -> [branch: "HEAD"]
        [{:branch, branch} | _] -> [branch: to_string(branch)]
        [{:tag, tag} | _]       -> [tag: to_string(tag)]
        [{:ref, ref} | _]       -> [ref: to_string(ref)]
        [ref | _]               -> [ref: to_string(ref)]
        _                       -> []
      end

    compile =
      if :proplists.get_value(:raw, opts, false),
        do: [compile: false],
        else: []

    mix_opts = [{scm, to_string(url)}] ++ ref ++ compile
    {app, compile_req(req), mix_opts}
  end

  defp compile_req(nil) do
    ">= 0.0.0"
  end

  defp compile_req(req) do
    case Regex.compile(List.to_string(req)) do
      {:ok, re} ->
        re
      {:error, reason} ->
        Mix.raise "Unable to compile version regex: #{inspect req}, #{reason}"
    end
  end

  defp manager_to_env(:rebar), do: "MIX_REBAR"
  defp manager_to_env(:rebar3), do: "MIX_REBAR3"

  defp eval_script(script_path, config) do
    script = Path.basename(script_path) |> String.to_charlist

    result = File.cd!(Path.dirname(script_path), fn ->
      :file.script(script, eval_binds(CONFIG: config, SCRIPT: script))
    end)

    case result do
      {:ok, config} ->
        config
      {:error, error} ->
        reason = :file.format_error(error)
        Mix.shell.error("Error evaluating Rebar config script #{script_path}:#{reason}")
        Mix.shell.error("Any dependencies defined in the script won't be available " <>
                        "unless you add them to your Mix project")
        config
    end
  end

  defp eval_binds(binds) do
    Enum.reduce(binds, :erl_eval.new_bindings, fn({k, v}, binds) ->
      :erl_eval.add_binding(k, v, binds)
    end)
  end

  defp wrap_cmd(rebar) do
    cond do
      not match?({:win32, _}, :os.type) ->
        rebar
      String.ends_with?(rebar, ".cmd") ->
        "\"#{String.replace(rebar, "/", "\\")}\""
      true ->
        "escript.exe \"#{rebar}\""
    end
  end

  defp apply_overrides(app, config, overrides) do
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
        {:override, oapp, overrides}, config when oapp == app ->
          Enum.reduce(overrides, config, fn {key, value}, config ->
            Keyword.put(config, key, value)
          end)
        _, config ->
          config
      end)

    Enum.reduce(overrides, config, fn
      {:add, oapp, overrides}, config when oapp == app ->
        Enum.reduce(overrides, config, fn {key, value}, config ->
          old_value = Keyword.get(config, key, [])
          Keyword.put(config, key, value ++ old_value)
      end)
      _, config ->
        config
    end)
  end
end
