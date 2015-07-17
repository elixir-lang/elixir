defmodule Mix.Tasks.Local.Rebar do
  use Mix.Task

  # @rebar_url should be moved to "http://s3.hex.pm/rebar/1/rebar"
  @rebar_url         "http://s3.hex.pm/rebar"
  @rebar_sha256_hash "c6108ac609f4e675fcfc3c57a5c791838faf82f017ff9325d0022c972649dd86"
  @shortdoc          "Install rebar locally"

  @moduledoc """
  Fetch a copy of rebar from the given path or url. It defaults to a
  rebar copy that ships with Elixir source if available or fetches it
  from #{@rebar_url}.

  The local copy is stored in your MIX_HOME (defaults to ~/.mix).
  This version of rebar will be used as required by `mix deps.compile`.

  ## Command line options

    * `--force` - forces installation without a shell prompt; primarily
      intended for automation in build systems like make
  """
  @spec run(OptionParser.argv) :: true
  def run(argv) do
    {opts, argv, _} = OptionParser.parse(argv, switches: [force: :boolean])

    path = case argv do
      []       -> @rebar_url
      [path|_] -> path
    end

    do_install(path, opts)
  end

  defp do_install(path, opts) do
    local_rebar_path = Mix.Rebar.local_rebar_path
    tmp_local_rebar_path = local_rebar_path <> ".tmp"

    File.rm(tmp_local_rebar_path)

    if Mix.Utils.copy_path!(path, tmp_local_rebar_path, opts) do
      if path == @rebar_url do
        try do
          downloaded_hash = (:crypto.hash(:sha256, File.read!(tmp_local_rebar_path)) |> Base.encode16 |> String.downcase)
          if (@rebar_sha256_hash != downloaded_hash), do:
            throw(downloaded_hash)
        catch
          hash -> raise "SHA256 hash did not match for `#{tmp_local_rebar_path}` [#{@rebar_url}].\nExpected: #{@rebar_sha256_hash}\nGot:      #{hash}"
        end
      end

      :ok = :file.rename tmp_local_rebar_path, local_rebar_path
      :ok = :file.change_mode local_rebar_path, 0o755
      Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(local_rebar_path)]
    end

    true
  end
end
