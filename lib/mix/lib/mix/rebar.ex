defmodule Mix.Rebar do
  @moduledoc false

  # Make Mix.Rebar work like a project so we can push it into the stack.
  @doc false
  def project, do: []

  @doc """
  Returns the path supposed to host the local copy of rebar.
  """
  def local_rebar_path, do: Path.join(Mix.Utils.mix_home, "rebar")

  @doc """
  Returns the path to the global copy of rebar, if one exists.
  """
  def global_rebar_cmd do
    wrap_cmd System.find_executable("rebar")
  end

  @doc """
  Returns the path to the local copy of rebar, if one exists.
  """
  def local_rebar_cmd do
    rebar = local_rebar_path
    wrap_cmd(if File.regular?(rebar), do: rebar)
  end

  @doc """
  Returns the path to the available rebar command.
  """
  def rebar_cmd do
    global_rebar_cmd || local_rebar_cmd
  end

  @doc """
  Loads the rebar.config and evaluates rebar.config.script if it
  exists in the given directory.
  """
  def load_config(dir) do
    config_path = Path.join(dir, "rebar.config")
    script_path = Path.join(dir, "rebar.config.script")

    config = case :file.consult(config_path) do
      { :ok, config } ->
        config
      { :error, :enoent } ->
        []
      { :error, error } ->
        reason = :file.format_error(error)
        raise Mix.Error, message: "Error consulting rebar config #{config_path}: #{reason}"
    end

    if File.exists?(script_path) do
      eval_script(script_path, config)
    else
      config
    end
  end

  @doc """
  Parses the dependencies in given rebar.config to Mix's dependency format.
  """
  def deps(config) do
    if deps = config[:deps] do
      deps_dir = config[:deps_dir] || "deps"
      Enum.map(deps, parse_dep(&1, deps_dir))
    else
      []
    end
  end

  @doc """
  Runs `fun` inside the given directory and all specified `sub_dirs` in the
  rebar config in the directory.
  """
  def recur(dir, fun) do
    config = load_config(dir)

    if sub_dirs = config[:sub_dirs] do
      sub_dirs = sub_dirs
       |> Enum.map(Path.wildcard(&1))
       |> List.concat
       |> Enum.filter(File.dir?(&1))

      Enum.map(sub_dirs, fn(dir) ->
        recur(dir, fun)
      end) |> List.concat
    end

    [File.cd!(dir, fn -> fun.(config) end)]
  end

  defp parse_dep({ app, req }, deps_dir) do
    { app, compile_req(req), [path: Path.join(deps_dir, app)] }
  end

  defp parse_dep({ app, req, source }, _deps_dir) do
    [ scm, url | source ] = tuple_to_list(source)

    { ref, source } = case source do
      [""|s]                  -> { [branch: "HEAD"], s }
      [{ :branch, branch }|s] -> { [branch: to_binary(branch)], s }
      [{ :tag, tag }|s]       -> { [tag: to_binary(tag)], s }
      [ref|s]                 -> { [ref: to_binary(ref)], s }
      _                       -> { [], [] }
    end

    raw = case source do
      [[:raw]|_]      -> [app: false]
      [[raw: true]|_] -> [app: false]
      _               -> []
    end

    opts = [{ scm, to_binary(url) }] ++ ref ++ raw
    { app, compile_req(req), opts }
  end

  defp parse_dep(app, deps_dir) do
    parse_dep({ app, ".*" }, deps_dir)
  end

  defp compile_req(req) do
    case to_binary(req) |> Regex.compile do
      { :ok, re } ->
        re
      { :error, reason } ->
        raise Mix.Error, message: "Unable to compile version regex: \"#{req}\", #{reason}"
    end
  end

  defp eval_script(script_path, config) do
    script = Path.basename(script_path) |> binary_to_list
    result = File.cd!(Path.dirname(script_path), fn ->
      :file.script(script, eval_binds(CONFIG: config, SCRIPT: script))
    end)
    case result do
      { :ok, config } ->
        config
      { :error, error } ->
        reason = :file.format_error(error)
        Mix.Shell.error("Error evaluating rebar config script #{script_path}: #{reason}")
        Mix.Shell.error("Any dependency defined in the script won't be available " <>
          "unless you add them to your Mix project")
        config
    end
  end

  defp eval_binds(binds) do
    Enum.reduce(binds, :erl_eval.new_bindings, fn ({k, v}, binds) ->
      :erl_eval.add_binding(k, v, binds)
    end)
  end

  defp wrap_cmd(nil), do: nil
  defp wrap_cmd(rebar) do
    if match?({ :win32, _ }, :os.type) and not String.ends_with?(rebar,".cmd") do
      "escript.exe #{rebar}"
    else
      rebar
    end
  end
end
