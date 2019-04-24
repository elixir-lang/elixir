defmodule Mix.Tasks.Compile.All do
  use Mix.Task.Compiler

  @moduledoc false
  @recursive true

  # This is an internal task used by "mix compile" which
  # is meant to be recursive and be invoked for each child
  # project.

  def run(args) do
    Mix.Project.get!()

    # Make sure Mix.Dep is cached to avoid loading dependencies
    # during compilation. It is likely this will be invoked anyway,
    # as both elixir and app compilers rely on it.
    Mix.Dep.cached()

    # Build the project structure so we can write down compiled files.
    Mix.Project.build_structure()

    with_logger_app(fn ->
      res = do_compile(Mix.Tasks.Compile.compilers(), args, :noop, [])
      true = Code.prepend_path(Mix.Project.compile_path())
      res
    end)
  end

  defp with_logger_app(fun) do
    app = Keyword.fetch!(Mix.Project.config(), :app)
    logger? = Process.whereis(Logger)
    logger_config_app = Application.get_env(:logger, :compile_time_application)

    try do
      if logger? do
        Logger.configure(compile_time_application: app)
      end

      fun.()
    after
      if logger? do
        Logger.configure(compile_time_application: logger_config_app)
      end
    end
  end

  defp do_compile([], _, status, diagnostics) do
    {status, diagnostics}
  end

  defp do_compile([compiler | rest], args, status, diagnostics) do
    {new_status, new_diagnostics} = run_compiler(compiler, args)
    diagnostics = diagnostics ++ new_diagnostics

    case new_status do
      :error ->
        if "--return-errors" not in args do
          exit({:shutdown, 1})
        end

        {:error, diagnostics}

      :ok ->
        do_compile(rest, args, :ok, diagnostics)

      :noop ->
        do_compile(rest, args, status, diagnostics)
    end
  end

  defp run_compiler(compiler, args) do
    Mix.Task.Compiler.normalize(Mix.Task.run("compile.#{compiler}", args), compiler)
  end
end
