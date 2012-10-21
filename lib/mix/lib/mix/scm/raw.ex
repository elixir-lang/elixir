defmodule Mix.SCM.Raw do
  @behavior Mix.SCM
  @moduledoc false

  def key do
    :raw
  end

  def consumes?(opts) do
    if raw = opts[:raw] do
      Keyword.put opts, :path, File.expand_path(raw)
    end
  end

  def available?(opts) do
    File.dir?(opts[:path])
  end

  def check?(_opts) do
    true
  end

  def match?(opts1, opts2) do
    opts1[:raw] == opts2[:raw]
  end

  def checkout(opts) do
    path = Mix.Utils.relative_to_cwd opts[:path]
    raise Mix.Error, message: "Cannot checkout raw dependency. Expected a dependency at #{path}"
  end

  def update(opts) do
    opts[:lock]
  end

  def clean(opts) do
    path = Mix.Utils.relative_to_cwd opts[:path]
    Mix.shell.info "  #{path} is a raw dependency, it was not cleaned"
    :noop
  end
end