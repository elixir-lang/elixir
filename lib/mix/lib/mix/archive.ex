defmodule Mix.Archive do
  @moduledoc """
  Module responsible for managing [archives](http://www.erlang.org/doc/man/code.html).

  An archive is a zip file containing the app and beam files.
  A valid archive must be named with the name of the application and
  it should contain the relative paths beginning with the application
  name, e.g. the root of the zip file should be `my_app/ebin/Elixir.My.App.beam`.
  """

  @doc """
  Returns the archive name based on `app` and `version`.

  ## Examples

      iex> Mix.Archive.name("foo", nil)
      "foo.ez"

      iex> Mix.Archive.name("foo", "0.1.0")
      "foo-0.1.0.ez"

  """
  def name(app, nil), do: "#{app}.ez"
  def name(app, vsn), do: "#{app}-#{vsn}.ez"

  @doc """
  Returns the archive internal directory from its `path`.

  ## Examples

      iex> Mix.Archive.dir("foo/bar/baz-0.1.0.ez")
      "baz-0.1.0"

  """
  def dir(path) do
    path |> Path.basename |> Path.rootname
  end

  @doc """
  Returns the ebin directory inside the given archive path.

  ## Examples

      iex> Mix.Archive.ebin("foo/bar/baz-0.1.0.ez")
      "foo/bar/baz-0.1.0.ez/baz-0.1.0/ebin"

  """
  def ebin(path) do
    dir = dir(path)
    Path.join [path, dir, "ebin"]
  end

  @doc """
  Creates an application archive.

  It receives the archive file in the format
  `path/to/archive/app-vsn.ez` and the path to the root of
  the project to be archived. Everything in the `ebin` and
  `priv` directories is archived. Dependencies are not
  archived.
  """
  def create(source, target) do
    source_path = Path.expand(source)
    target_path = Path.expand(target)
    dir = dir(target_path) |> List.from_char_data!
    {:ok, _} = :zip.create(target_path,
                  files_to_add(source_path, dir),
                  uncompress: ['.beam', '.app'])
  end

  defp files_to_add(path, dir) do
    File.cd! path, fn ->
      ebin = :filelib.wildcard('ebin/*.{beam,app}')
      priv = :filelib.wildcard('priv/**/*')

      Enum.reduce ebin ++ priv, [], fn(f, acc) ->
        case File.read(f) do
          {:ok, bin} ->
            [{:filename.join(dir, f), bin}|acc]
          {:error, _} ->
            acc
        end
      end
    end
  end
end
