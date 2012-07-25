defmodule Mix.Generator do
  @moduledoc """
  Conveniences for working with paths and generating content.
  """

  @doc """
  Creates a file with the given contents.
  If the file already exists, asks for user confirmation.
  """
  def create_file(path, contents) do
    Mix.shell.info "* creating #{path}"

    if overriding?(path) do
      File.write! path, contents
    end
  end

  @doc """
  Creates a directory if one does not exist yet.
  """
  def create_directory(path) do
    Mix.shell.info "* creating #{path}"
    File.mkdir_p! path
  end

  defp overriding?(path) do
    if File.exists?(path) do
      full = File.expand_path(path)
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
      File.read! File.expand_path(unquote(path), __FILE__)
    end
  end

  @doc """
  Embed a template given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_template` that expects assigns as arguments.

  This function can be invoked passing no argument or passing
  a keywords list. Each key in the keyword list can be accessed
  in the template using the `@` macro.

  For more information, check `EEx.SmartEngine`.
  """
  defmacro embed_template(name, contents) do
    quote do
      require EEx
      EEx.function_from_string :defp, :"#{unquote(name)}_template", "<% @_abc  %>" <> unquote(contents), [:assigns]
    end
  end

  @doc """
  Embed a file given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_file` that expects no argument.
  """
  defmacro embed_file(name, contents) do
    quote do
      defp :"#{unquote(name)}_file", [], [], do: unquote(contents)
    end
  end
end