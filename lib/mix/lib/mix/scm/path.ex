defmodule Mix.SCM.Path do
  @behavior Mix.SCM
  @moduledoc false

  def format(opts) do
    opts[:path]
  end

  def format_lock(_lock) do
    nil
  end

  def accepts_options(app, opts) do
    cond do
      raw = opts[:path] ->
        Keyword.put opts, :dest, Path.expand(raw)
      opts[:in_umbrella] ->
        path = "../#{app}"

        opts
          |> Keyword.put(:dest, Path.expand(path))
          |> Keyword.put_new(:path, path)
          |> Keyword.put_new(:env, Mix.env)
      true ->
        nil
    end
  end

  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  def lock_status(_opts) do
    :ok
  end

  def equal?(opts1, opts2) do
    opts1[:dest] == opts2[:dest]
  end

  def checkout(opts) do
    path = Path.relative_to_cwd opts[:dest]
    raise Mix.Error, message: "Cannot checkout path dependency, expected a dependency at #{path}"
  end

  def update(opts) do
    opts[:lock]
  end

  def clean(opts) do
    path = Path.relative_to_cwd opts[:dest]
    Mix.shell.info "  #{path} is a path dependency, it was not cleaned"
    :noop
  end
end