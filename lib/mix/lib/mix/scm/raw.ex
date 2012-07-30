defmodule Mix.SCM.Raw do
  @behavior Mix.SCM

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

  def get(_path, opts) do
    opts[:lock]
  end

  def update(_path, _opts) do
    nil
  end

  def clean(path, _) do
    Mix.shell.info "  #{path} is a raw dependency, it was not cleaned"
    :noop
  end
end