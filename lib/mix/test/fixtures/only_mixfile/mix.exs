defmodule MyProject do
  use Mix.Project

  def project do
    []
  end

  def hello_world do
    "Hello from MyProject!"
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task

  @shortdoc "Hello"

  def run(_) do
    IO.puts Mix.Project.current.hello_world
  end
end

defmodule Mix.Tasks.Invalid do
end