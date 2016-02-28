defmodule Mix.Local.Installer do
  @moduledoc """
  This module implements pieces of functionality shared by the archive- and escript-related
  tasks.
  """

  @doc """
  Checks that the argument given to install is supported by the respective module.
  """
  @callback check_path_or_url(String.t) :: :ok | {:error, String.t}

  @doc """
  Returns a list of already installed version of the same archive or escript.
  """
  @callback find_previous_versions(String.t, Path.t) :: [Path.t]

  @doc """
  Custom actions to be performed before the actual installation.
  """
  @callback before_install(String.t, Path.t) :: :ok | {:error, String.t}

  @doc """
  Custom actions to be performed after the installation has succeeded.
  """
  @callback after_install(Path.t, [Path.t]) :: term

  @doc """
  Common implementation of installation for archives and escripts.

  Relies on a few callbacks provided by respective callback modules
  for customizing certain steps in the installation process.
  """
  @spec install({module, atom}, OptionParser.argv, Keyword.t) :: boolean
  def install({module, name}, argv, switches) do
    {opts, args, _} = OptionParser.parse(argv, switches: switches)

    case args do
      [src] ->
        with :ok <- check_argument(src), :ok <- module.check_path_or_url(src) do
          do_install({module, name}, src, opts)
        else
          {:error, message} -> Mix.raise message <> "\n" <> usage(name)
        end

      [] ->
        src = Mix.Local.name_for(name, Mix.Project.config)
        if File.exists?(src) do
          do_install({module, name}, src, opts)
        else
          Mix.raise "Expected an #{name} to exist in the current directory " <>
                    "or an argument to be given.\n#{usage(name)}"
        end

      _ ->
        Mix.raise "Unexpected arguments.\n#{usage(name)}"
    end
  end

  defp check_argument(arg) do
    if local_path?(arg) or file_url?(arg) do
      :ok
    else
      {:error, "Expected a local file path or a file URL."}
    end
  end

  defp local_path?(url_or_path) do
    File.regular?(url_or_path)
  end

  defp file_url?(url_or_path) do
    URI.parse(url_or_path).scheme in ["http", "https"]
  end

  defp usage(name), do: "Usage: mix #{name}.install <path or url>"

  defp do_install({module, name}, src, opts) do
    src_basename = Path.basename(URI.parse(src).path)
    dst_file_path = Path.join(Mix.Local.path_for(name), src_basename)
    dst_dir_path = Path.dirname(dst_file_path)
    previous_files = module.find_previous_versions(src, dst_file_path)

    if opts[:force] || should_install?(name, src, previous_files) do
      case module.before_install(src, dst_file_path) do
        :ok -> :ok
        {:error, message} -> Mix.raise message
      end

      case Mix.Utils.read_path(src, opts) do
        {:ok, binary} ->
          File.mkdir_p!(dst_dir_path)
          File.write!(dst_file_path, binary)

        :badpath ->
          Mix.raise "Expected #{inspect src} to be a URL or a local file path"

        {:local, message} ->
          Mix.raise message

        {kind, message} when kind in [:remote, :checksum] ->
          Mix.raise """
          #{message}

          Could not fetch #{name} at:

              #{src}

          Please download the #{name} above manually to your current directory and run:

              mix #{name}.install ./#{src_basename}
          """
      end

      Mix.shell.info [:green, "* creating ", :reset, Path.relative_to_cwd(dst_file_path)]
      _ = module.after_install(dst_file_path, previous_files)
      true
    else
      false
    end
  end

  defp should_install?(name, src, previous_files) do
    message = case previous_files do
      [] ->
        "Are you sure you want to install #{name} #{inspect src}?"
      [file] ->
        "Found existing #{name}: #{file}.\n" <>
        "Are you sure you want to replace it with #{inspect src}?"
      files ->
        "Found existing #{name}s: #{Enum.map_join(files, ", ", &Path.basename/1)}.\n" <>
        "Are you sure you want to replace them with #{inspect src}?"
    end
    Mix.shell.yes?(message)
  end

  @doc """
  Print a list of items in a uniform way. Used for printing the list of installed archives and
  escripts.
  """
  @spec print_list(atom, [String.t]) :: :ok
  def print_list(type, []) do
    Mix.shell.info "No #{type}s currently installed."
  end

  def print_list(type, items) do
    Enum.each items, fn item -> Mix.shell.info ["* ", item] end
    item_name = String.capitalize("#{type}")
    Mix.shell.info "#{item_name}s installed at: #{Mix.Local.path_for(type)}"
  end

  @doc """
  A common implementation for uninstalling archives and scripts.
  """
  @spec uninstall(atom, OptionParser.argv) :: boolean
  def uninstall(type, argv) do
    {_, argv, _} = OptionParser.parse(argv)

    item_name = "#{type}"
    item_plural = "#{type}s"
    root = Mix.Local.path_for(type)

    if name = List.first(argv) do
      path = Path.join(root, name)
      cond do
        not File.regular?(path) ->
          Mix.shell.error "Could not find a local #{item_name} named #{inspect name}. "<>
                          "Existing #{item_plural} are:"
          Mix.Task.run item_name
          nil
        should_uninstall?(path, item_name) ->
          File.rm!(path)
          path
        true ->
          nil
      end
    else
      Mix.raise "No #{item_name} was given to #{item_name}.uninstall"
    end
  end

  defp should_uninstall?(path, item_name) do
    Mix.shell.yes?("Are you sure you want to uninstall #{item_name} #{path}?")
  end
end
