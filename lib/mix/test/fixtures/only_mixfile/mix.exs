defmodule MyProject do
  use Mix.Project

  def hello_world do
    "Hello from MyProject!"
  end
end

defmodule Mix.Tasks.Hello do
  use Mix.Task

  def run(_) do
    IO.puts Mix.Project.current.hello_world
  end
end
