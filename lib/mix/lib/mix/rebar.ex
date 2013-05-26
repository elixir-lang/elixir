defmodule Mix.Rebar do
  @moduledoc false

  @doc """
  Loads the rebar.config and evaluates rebar.config.script if it exists in the
  given directory.
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
      Enum.map(deps, fn({ app, req, source }) ->
        { scm, url } = case source do
          { scm, url } -> { scm, to_binary(url) }
          { scm, url, _ } -> { scm, to_binary(url) }
        end

        opts = case source do
          { _, _, "" }                  -> [branch: "HEAD"]
          { _, _, { :branch, branch } } -> [branch: to_binary(branch)]
          { _, _, { :tag, tag } }       -> [tag: to_binary(tag)]
          { _, _, ref }                 -> [ref: to_binary(ref)]
          _                             -> []
        end
        { app, to_binary(req), [{scm, to_binary(url)}|opts] }
      end)
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
      sub_dirs =  sub_dirs
               |> Enum.map(Path.wildcard(&1))
               |> List.concat
               |> Enum.filter(File.dir?(&1))

      Enum.map(sub_dirs, fn(dir) ->
        recur(dir, fun)
      end) |> List.concat
    end

    [File.cd!(dir, fn -> fun.(config) end)]
  end

  defp eval_script(path, config) do
    script = Path.basename(path)
    case :file.script(path, eval_binds(CONFIG: config, SCRIPT: script)) do
      { :ok, config } ->
        config
      { :error, error } ->
        reason = :file.format_error(error)
        raise Mix.Error, message: "Error evaluating rebar config script #{path}: #{reason}"
    end
  end

  defp eval_binds(binds) do
    Enum.reduce(binds, :erl_eval.new_bindings, fn ({k, v}, binds) ->
      :erl_eval.add_binding(k, v, binds)
    end)
  end
end
