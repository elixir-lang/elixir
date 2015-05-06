defmodule Mix.Generator do
  @moduledoc """
  Conveniences for working with paths and generating content.

  All of those functions are verbose, in the sense they log
  the action to be performed via `Mix.shell`.
  """

  @doc """
  Creates a file with the given contents.
  If the file already exists, asks for user confirmation.

  ## Options

    * `:force` - forces installation without a shell prompt.
  """
  def create_file(path, contents, opts \\ []) when is_binary(path) do
    Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(path)]

    if opts[:force] || Mix.Utils.overwriting?(path) do
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, contents)
    end
  end

  @doc """
  Creates a directory if one does not exist yet.
  """
  def create_directory(path) when is_binary(path) do
    Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(path)]
    File.mkdir_p! path
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
    quote bind_quoted: binding do
      contents =
        case contents do
          [from_file: file] ->
            @file file
            File.read!(file)
          c when is_binary(c) ->
            @file {__ENV__.file, __ENV__.line+1}
            c
          _ ->
            raise ArgumentError, "expected string or from_file: file"
        end

      require EEx
      EEx.function_from_string :defp, :"#{name}_template", "<% _ = assigns %>" <> contents, [:assigns]
    end
  end

  @doc """
  Embeds a text given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_text` that expects no argument.
  """
  defmacro embed_text(name, contents) do
    quote bind_quoted: binding do
      contents =
        case contents do
          [from_file: f] -> File.read!(f)
          c when is_binary(c) -> c
          _ -> raise ArgumentError, "expected string or from_file: file"
        end
      defp unquote(:"#{name}_text")(), do: unquote(contents)
    end
  end
end
