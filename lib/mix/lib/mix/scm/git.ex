defmodule Mix.SCM.Git do
  def cloned?(path) do
    File.dir?(File.join(path, ".git"))
  end
end