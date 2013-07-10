defmodule MyProject do
  use Mix.Project

  def project do
    [ app: :no_mixfile,
      version: "0.1" ]
  end

  def hello_world do
    "Hello from MyProject!"
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task

  @shortdoc "Hello"

  def run(_) do
    IO.puts Mix.Project.get!.hello_world
  end
end

defmodule Mix.Tasks.Invalid do
end