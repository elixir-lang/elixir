defmodule Mix.SCM.Raw do
  @behavior Mix.SCM
  @moduledoc false

  def key do
    :raw
  end

  def consumes?(opts) do
    if raw = opts[:raw] do
      Keyword.put opts, :path, raw
    end
  end

  def available?(path, _) do
    File.dir?(path)
  end

  def check?(_app, _opts) do
    true
  end

  def match?(opts1, opts2) do
    opts1[:raw] == opts2[:raw]
  end

  def checkout(path, _opts) do
    raise Mix.Error, message: "Cannot checkout raw dependency. Expected a dependency at #{path}"
  end

  def update(_path, opts) do
    opts[:lock]
  end

  def clean(path, _) do
    Mix.shell.info "  #{path} is a raw dependency, it was not cleaned"
    :noop
  end
end