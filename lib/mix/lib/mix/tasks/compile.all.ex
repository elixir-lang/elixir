defmodule Mix.Tasks.Compile.All do
  use Mix.Task

  @moduledoc false
  @recursive true

  # This is an internal task used by mix compile which
  # is meant to be recursive and be invoked for each child
  # project.

  def run(args) do
    Mix.Project.get!

    # Build the project structure so we can write down compiled files.
    Mix.Project.build_structure

    with_logger_app fn ->
      res =
        Enum.map(Mix.Tasks.Compile.compilers(), fn(compiler) ->
          Mix.Task.run("compile.#{compiler}", args)
        end)

      true = Code.prepend_path(Mix.Project.compile_path)
      if :ok in res, do: :ok, else: :noop
    end
  end

  defp with_logger_app(fun) do
    app = Keyword.fetch!(Mix.Project.config, :app)
    logger? = Process.whereis(Logger)
    logger_config_app = Application.get_env(:logger, :compile_time_application)

    try do
      if logger? do
        Logger.configure([compile_time_application: app])
      end
      fun.()
    after
      if logger? do
        Logger.configure([compile_time_application: logger_config_app])
      end
    end
  end
end
