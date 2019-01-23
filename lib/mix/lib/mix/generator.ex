defmodule Mix.Generator do
  @moduledoc """
  Conveniences for working with paths and generating content.
  """

  @doc ~S"""
  Creates a file with the given contents.

  If the file already exists and the contents are not the same,
  it asks for user confirmation.

  ## Options

    * `:force` - forces installation without a shell prompt
    * `:quiet` - do not log command output

  ## Examples

      iex> Mix.Generator.create_file(".gitignore", "_build\ndeps\n")
      * creating .gitignore
      :ok

  """
  @spec create_file(Path.t(), iodata, keyword) :: boolean()
  def create_file(path, contents, opts \\ []) when is_binary(path) do
    log(:green, :creating, Path.relative_to_cwd(path), opts)

    if opts[:force] || overwrite?(path, contents) do
      File.mkdir_p!(Path.dirname(path))
      File.write!(path, contents)
      true
    else
      false
    end
  end

  @doc """
  Creates a directory if one does not exist yet.

  This function does nothing if the given directory already exists; in this
  case, it still logs the directory creation.

  ## Options

    * `:quiet` - do not log command output

  ## Examples

      iex> Mix.Generator.create_directory("path/to/dir")
      * creating path/to/dir
      :ok

  """
  @spec create_directory(Path.t(), keyword) :: true
  def create_directory(path, options \\ []) when is_binary(path) do
    log(:green, "creating", Path.relative_to_cwd(path), options)
    File.mkdir_p!(path)
    true
  end

  @doc """
  Prompts the user to overwrite the file if it exists.

  Returns false if the file exists and the user forbade
  to override it. Returns true otherwise.
  """
  @doc since: "1.9.0"
  @spec overwrite?(Path.t(), iodata) :: boolean
  def overwrite?(path) do
    if File.exists?(path) do
      full = Path.expand(path)
      Mix.shell().yes?(Path.relative_to_cwd(full) <> " already exists, overwrite?")
    else
      true
    end
  end

  @doc """
  Prompts the user to overwrite the file if it exists.

  The contents are compared to avoid asking the user to
  override if the contents did not change. Returns false
  if the file exists and the content is the same or the
  user forbade to override it. Returns true otherwise.
  """
  @doc since: "1.9.0"
  @spec overwrite?(Path.t(), iodata) :: boolean
  def overwrite?(path, contents) do
    case File.read(path) do
      {:ok, binary} ->
        if binary == IO.iodata_to_binary(contents) do
          false
        else
          full = Path.expand(path)
          Mix.shell().yes?(Path.relative_to_cwd(full) <> " already exists, overwrite?")
        end

      _ ->
        true
    end
  end

  defp log(color, command, message, opts) do
    unless opts[:quiet] do
      Mix.shell().info([color, "* #{command} ", :reset, message])
    end
  end

  @doc """
  Embeds a template given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_template` that expects assigns as arguments.

  This function must be invoked passing a keyword list.
  Each key in the keyword list can be accessed in the
  template using the `@` macro.

  For more information, check `EEx.SmartEngine`.

  ## Examples

      defmodule Mix.Tasks.MyTask do
        require Mix.Generator
        Mix.Generator.embed_template(:log, "Log: <%= @log %>")
      end

  """
  defmacro embed_template(name, contents) do
    quote bind_quoted: binding() do
      contents =
        case contents do
          [from_file: file] ->
            @file file
            File.read!(file)

          c when is_binary(c) ->
            @file {__ENV__.file, __ENV__.line + 1}
            c

          _ ->
            raise ArgumentError, "expected string or from_file: file"
        end

      require EEx

      source = "<% _ = assigns %>" <> contents
      EEx.function_from_string(:defp, :"#{name}_template", source, [:assigns])
    end
  end

  @doc """
  Embeds a text given by `contents` into the current module.

  It will define a private function with the `name` followed by
  `_text` that expects no arguments.

  ## Examples

      defmodule Mix.Tasks.MyTask do
        require Mix.Generator
        Mix.Generator.embed_text(:error, "There was an error!")
      end

  """
  defmacro embed_text(name, contents) do
    quote bind_quoted: binding() do
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
