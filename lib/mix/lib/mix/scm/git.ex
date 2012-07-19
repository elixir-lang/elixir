defmodule Mix.SCM.Git do
  def available?(path) do
    File.dir?(File.join(path, ".git"))
  end
end