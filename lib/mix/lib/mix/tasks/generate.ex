defmodule Mix.Tasks.Generate do
  use Mix.Task

  import Mix.Generator
  import Mix.Templates

  @shortdoc "Generate boilerplate code from templates"

  @moduledoc """
  Generate boilerplate code from templates

    mix generate GENERATOR CAMELISED_NAME

  Where GENERATOR is one of:

    module   Generate a module

  And CAMELISED_NAME is a module name like MyModuleName or
  MyNameSpace.MyModuleName.
  """

  def run(["module", module_name]) do
    create_file module_file_path(module_name), lib_template([mod: module_name])
  end

  defp module_file_path(module_name) do
    Path.join("lib", "#{Mix.Utils.underscore(module_name)}.ex")
  end
end
