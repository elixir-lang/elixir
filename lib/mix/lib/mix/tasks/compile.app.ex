defmodule Mix.Tasks.Compile.App do
  use Mix.Task

  @hidden true
  @shortdoc "Write .app file"

  @moduledoc """
  Writes an .app file.

  By default, this task will detect all modules in your compile_path
  (default to "ebin") and generate a best guess for your application
  specification. This best guess also includes "kernel", "stdlib"
  and "elixir" as application dependencies.

  You can optionally define an `application/0` function inside your
  `Mix.Project` that returns a keyword list to further configure
  your application according to OTP design principles:

  http://www.erlang.org/doc/design_principles/applications.html

  ## Configuration
  
  * `:app` - The application name as a binary (required)
  * `:version` - The application version as a binary (required)

  ## Command line options

  * `--force` forces compilation regardless of mod times

  """
  def run(args) do
    { opts, _ } = OptionParser.parse(args, flags: [:force])

    project = Mix.Project.get!
    config  = Mix.project

    app     = Keyword.get!(config, :app)
    version = Keyword.get!(config, :version)

    validate_app(app)
    validate_version(version)

    path    = config[:compile_path] || "ebin"
    beams   = File.wildcard('#{path}/*.beam')

    target  = File.join(path, "#{app}.app")
    sources = [Mix.Utils.source(project) | beams]

    if opts[:force] or Mix.Utils.stale?(sources, [target]) do
      best_guess = [
        vsn: to_char_list(version),
        modules: modules_from(beams),
        applications: [:kernel, :stdlib, :elixir]
      ]

      properties = if function_exported?(project, :application, 0) do
        Mix.Utils.config_merge(best_guess, project.application)
      else
        best_guess
      end

      properties = ensure_correct_properties(app, properties)
      contents   = { :application, app, properties }

      File.mkdir_p!(File.dirname(target))
      file = File.open!(target, [:write])
      :io.fwrite(file, "~p.", [contents])
      File.close(file)

      Mix.shell.info "Generated #{app}.app"
      :ok
    else
      :noop
    end
  end

  defp validate_app(app) when is_atom(app), do: :ok
  defp validate_app(_), do: raise(Mix.Error, message: "Expected :app to be an atom")

  defp validate_version(version) when is_binary(version), do: :ok
  defp validate_version(_), do: raise(Mix.Error, message: "Expected :version to be a binary")

  defp modules_from(beams) do
    Enum.map beams, &1 /> File.basename /> File.rootname('.beam') /> list_to_atom
  end

  defp ensure_correct_properties(app, properties) do
    properties = Keyword.from_enum(properties)
    properties = Keyword.put properties, :description, 
                             to_char_list(properties[:description] || app)
    properties = Keyword.put properties, :registered, (properties[:registered] || [])
    validate_properties(properties)
    properties
  end

  defp validate_properties(properties) do
    unless nil?(properties[:description]) or is_list(properties[:description]) do
      raise(Mix.Error, message: "Application description (:description) is not a character list (got #{inspect properties[:description]})")
    end
    unless nil?(properties[:id]) or is_list(properties[:id]) do
      raise(Mix.Error, message: "Application id (:id) is not a character list (got #{inspect properties[:id]} instead)")
    end    
    unless nil?(properties[:vsn]) or is_list(properties[:vsn]) do
      raise(Mix.Error, message: "Application vsn (:vsn) is not a character list (got #{inspect properties[:vsn]} instead)")
    end
    unless nil?(properties[:modules]) or (is_list(properties[:modules]) and Enum.all?(properties[:modules], is_atom(&1))) do
      raise(Mix.Error, message: "Application modules (:modules) should be a list of atoms (got #{inspect properties[:modules]} instead)")
    end
    unless nil?(properties[:maxT]) or properties[:maxT] == :infinity or is_integer(properties[:maxT]) do
      raise(Mix.Error, message: "Application maximum time (:maxT) is not an integer or :infinity (got #{inspect properties[:maxT]} instead)")
    end
    unless nil?(properties[:registered]) or is_list(properties[:registered]) and (Enum.all?(properties[:registered], is_atom(&1))) do
      raise(Mix.Error, message: "Application registered processes (:registered) should be a list of atoms (got #{inspect properties[:registered]} instead)")
    end
    unless nil?(properties[:included_applications]) or (is_list(properties[:included_applications]) and Enum.all?(properties[:included_applications], is_atom(&1))) do
      raise(Mix.Error, message: "Application included applications (:included_applications) should be a list of atoms (got #{inspect properties[:included_applications]} instead)")
    end
    unless nil?(properties[:applications]) or (is_list(properties[:applications]) and Enum.all?(properties[:applications], is_atom(&1))) do
      raise(Mix.Error, message: "Application dependencies (:applications) should be a list of atoms (got #{inspect properties[:applications]} instead)")
    end
    unless nil?(properties[:env]) or (Keyword.keyword?(properties[:env])) do
      raise(Mix.Error, message: "Application dependencies (:env) should be a keyword list (got #{inspect properties[:env]} instead)")
    end
    unless nil?(properties[:mod]) do
      case properties[:mod] do
        [] -> :ok
        {module, _start_args} when is_atom(module) -> :ok
        other ->
          raise(Mix.Error, message: "Application callback module (:mod) should be either [] or {module, start_args} (got #{inspect properties[:mod]} instead)")
      end          
    end
    unless nil?(properties[:start_phases]) or (Keyword.keyword?(properties[:start_phases])) do
      raise(Mix.Error, message: "Application start phases (:start_phases) should be a keyword list (got #{inspect properties[:start_phases]} instead)")
    end
  end
end
