defmodule Mix.Generator do
  @moduledoc """
  Conveniences for working with paths and generating content.

  All of those functions are verbose, in the sense they log
  the action to be performed via `Mix.shell`.
  """

  @doc """
  Creates a file with the given contents.
  If the file already exists, asks for user confirmation.
  """
  def create_file(path, contents) do
    Mix.shell.info "%{green}* creating%{reset} #{path}"

    if overwriting?(path) do
      File.write! path, contents
    end
  end

  @doc """
  Creates a directory if one does not exist yet.
  """
  def create_directory(path) do
    Mix.shell.info "%{green}* creating%{reset} #{path}"
    File.mkdir_p! path
  end

  defp overwriting?(path) do
    if File.exists?(path) do
      full = Path.expand(path)
      Mix.shell.yes?(full <> " already exists, overwrite?")
    else
      true
    end
  end

  @doc """
  Reads the content from a file relative to the current
  file and not relative to the cwd. Useful when used with
  embed macros:

      embed_template :lib, from_file("../templates/lib.eex")

  """
  defmacro from_file(path) do
    quote do
      File.read! Path.expand(unquote(path), __FILE__)
    end
  end

  @doc """
  Embed a template given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_template` that expects assigns as arguments.

  This function must be invoked passing a keyword list.
  Each key in the keyword list can be accessed in the
  template using the `@` macro.

  For more information, check `EEx.SmartEngine`.
  """
  defmacro embed_template(name, contents) do
    quote do
      require EEx
      EEx.function_from_string :defp, :"#{unquote(name)}_template", "<% _ = assigns %>" <> unquote(contents), [:assigns]
    end
  end

  @doc """
  Embeds a text given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_text` that expects no argument.
  """
  defmacro embed_text(name, contents) do
    quote do
      defp :"#{unquote(name)}_text", [], [], do: unquote(contents)
    end
  end
end
