defmodule Mix.Tasks.New.Task do
  use Mix.Task
  import Mix.Generator

  @impl true
  def run(argv) do
    case argv do
      [] ->
        Mix.raise("Expected TASK_NAME to be given, please use \"mix new.task TASK_NAME\"")

      [task_name] ->
        assigns = %{
          command: task_name,
          module:
            "Mix.Tasks.#{task_name}"
            |> String.split(".")
            |> Enum.map_join(".", &Macro.camelize/1)
        }

        path = "lib/mix/tasks/#{task_name}.ex"

        unless File.dir?("lib/mix/tasks") do
          create_directory("lib/mix/tasks")
        end

        create_file(path, task_template(assigns))

        """

        Your Mix task was created successfully.

        You can invoke your task by compiling your project and running:

            mix #{task_name}

        Run "mix help #{task_name}" to view the documentation.
        """
        |> String.trim_trailing()
        |> Mix.shell().info()
    end
  end

  embed_template(:task, """
  defmodule <%= @module %> do
    use Mix.Task

    @shortdoc "A placeholder shortdoc for mix <%= @command %>"
    @moduledoc @shortdoc

    @doc false
    def run(argv) do
      # Fill me in!
    end
  end
  """)
end
