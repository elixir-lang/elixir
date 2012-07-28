defmodule Mix.Tasks.New do
  use Mix.Task

  import Mix.Generator
  import Mix.Utils, only: [camelize: 1, underscore: 1]

  @shortdoc "Creates a new Elixir project"

  @moduledoc """
  Creates a new Elixir project.
  It expects the path of the project as argument.

      mix new PATH [--app APP] [--module MODULE]

  A project with the given path name will be created,
  unless `--app` is given, allowing you to set the app
  name or the `--module` is given configuring the module
  name.

  ## Examples

      mix new hello_world

  Is equivalent to:
  
      mix new hello_world --app hello_world --module HelloWorld

  """
  def run(argv) do
    { opts, argv } = OptionParser.parse(argv)
    case argv do
      [] ->
        raise Mix.Error, message: "expected PATH to be given, please use `mix new PATH`"
      [path|_] ->
        name = opts[:app] || File.basename(File.expand_path(path))
        check_project_name!(name)
        File.mkdir_p!(path)
        File.cd!(path, fn -> do_generate(underscore(name), opts) end)
    end
  end

  defp do_generate(app, opts) do
    mod     = opts[:module] || camelize(app)
    assigns = [app: app, mod: mod]

    create_file "README.md",  readme_template(assigns)
    create_file ".gitignore", gitignore_text
    create_file "mix.exs",    mixfile_template(assigns)

    create_directory "lib"
    create_file "lib/#{app}.ex", lib_template(assigns)

    create_directory "test"
    create_file "test/test_helper.exs", test_helper_template(assigns)
    create_file "test/#{app}_test.exs", test_lib_template(assigns)
  end

  defp check_project_name!(name) do
    unless name =~ %r/^[a-z][\w_]+$/i do
      raise Mix.Error, message: "project path must start with a letter and have only letters, numbers and underscore"
    end
  end

   embed_template :readme, """
   # <%= @mod %>

   ** TODO: Add description **
   """

   embed_text :gitignore, """
   /ebin
   /deps
   erl_crash.dump
   """

  embed_template :mixfile, """
  defmodule <%= @mod %>.MixFile do
    use Mix.Project

    def project do
      [ app: :<%= @app %>,
        version: "0.0.1",
        deps: deps ]
    end

    # Configuration for the OTP application
    def application do
      []
    end

    # Returns the list of dependencies in the format:
    # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
    defp deps do
      []
    end
  end
  """

  embed_template :lib, """
  defmodule <%= @mod %> do
    def start do
      :ok = :application.start(:<%= @app %>)
    end
  end
  """

  embed_template :test_lib, """
  Code.require_file "../test_helper", __FILE__

  defmodule <%= @mod %>Test do
    use ExUnit.Case

    test "the truth" do
      assert true
    end
  end
  """

  embed_template :test_helper, """
  <%= @mod %>.start
  ExUnit.start
  """
end
