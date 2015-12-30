defmodule Mix.Local.Installer do
  @moduledoc """
  This module implements pieces of functionality shared by the archive- and escript-related
  tasks.
  """

  @doc """
  Print a list of items in a uniform way. Used for printing the list of installed archives and
  escripts.

  ## Options

    * `:empty_message` - the message to print when there are no items
    * `:footnote` - the message to print after the list

  """
  def print_list([], options) do
    Mix.shell.info Keyword.get(options, :empty_message, "No items found.")
  end

  def print_list(items, options) do
    Enum.each items, fn item -> Mix.shell.info ["* ", item] end
    Mix.shell.info Keyword.get(options, :footnote, "")
  end

  @doc """
  A common implementation for uninstalling archives, scripts, etc.

  ## Options

    * `:item_name` - the name of the item being uninstalled. Also the name of the task which is used
    to print a list of all such items
    * `:item_plural` - plural of item name

  """
  def uninstall(argv, root, options) do
    {_, argv, _} = OptionParser.parse(argv)

    item_name = Keyword.fetch!(options, :item_name)
    item_plural = Keyword.fetch!(options, :item_plural)

    if name = List.first(argv) do
      path = Path.join(root, name)
      if File.regular?(path) do
        if should_uninstall?(path, item_name), do: File.rm!(path)
      else
        Mix.shell.error "Could not find a local #{item_name} named #{inspect name}. "<>
                        "Existing #{item_plural} are:"
        Mix.Task.run item_name
      end
    else
      Mix.raise "No #{item_name} was given to #{item_name}.uninstall"
    end
  end

  @doc """
  Parse `path_or_url` as a URI and return its base name.
  """
  def basename(path_or_url) do
    if path = URI.parse(path_or_url).path do
      Path.basename(path)
    else
      Mix.raise "Expected #{inspect path_or_url} to be a url or a local file path"
    end
  end

  defp should_uninstall?(path, item_name) do
    Mix.shell.yes?("Are you sure you want to uninstall #{item_name} #{path}?")
  end
end
