defmodule Mix.Tasks.Generate do
  use Mix.Task

  import Mix.Generator
  import Mix.Templates

  def run(["module", module_name]) do
    create_file module_file_path(module_name), lib_template([mod: module_name])
  end

  defp module_file_path(module_name) do
    Path.join("lib", "#{Mix.Utils.underscore(module_name)}.ex")
  end
end
