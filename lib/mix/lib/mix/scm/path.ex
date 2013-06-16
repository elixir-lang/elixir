defmodule Mix.SCM.Path do
  @behavior Mix.SCM
  @moduledoc false

  def format(opts) do
    [path: opts[:path]]
  end

  def format_lock(_lock) do
    nil
  end

  def accepts_options(opts) do
    if raw = opts[:path] do
      Keyword.put opts, :dest, Path.expand(raw)
    end
  end

  def checked_out?(opts) do
    File.dir?(opts[:dest])
  end

  def matches_lock?(_opts) do
    true
  end

  def equal?(opts1, opts2) do
    opts1[:dest] == opts2[:dest]
  end

  def checkout(opts) do
    path = Mix.Utils.relative_to_cwd opts[:dest]
    raise Mix.Error, message: "Cannot checkout path dependency. Expected a dependency at #{path}"
  end

  def update(opts) do
    opts[:lock]
  end

  def clean(opts) do
    path = Mix.Utils.relative_to_cwd opts[:dest]
    Mix.shell.info "  #{path} is a path dependency, it was not cleaned"
    :noop
  end
end