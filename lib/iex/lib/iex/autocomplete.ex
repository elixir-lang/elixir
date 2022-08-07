defmodule IEx.Autocomplete do
  @moduledoc false

  require Logger

  @doc """
  Provides one helper function that is injected into connecting
  remote nodes to properly handle autocompletion.
  """
  def remsh(node) do
    fn e ->
      case :rpc.call(node, IEx.Autocomplete, :expand, [e, IEx.Broker.shell()]) do
        {:badrpc, _} -> {:no, '', []}
        r -> r
      end
    end
  end

  @doc """
  The expansion logic.

  Some of the expansion has to use the current shell
  environment, which is found via the broker.
  """
  def expand(code, shell \\ IEx.Broker.shell()) do
    path_fragment = IEx.Autocomplete.Path.expandable_fragment(code)

    result =
      case path_fragment do
        [] -> IEx.Autocomplete.Code.expand(code, shell)
        _path -> IEx.Autocomplete.Path.expand(code, shell)
      end

    # Logger.debug(
    #   "Autocomplete Code='#{inspect(code)}', path_fragment='#{inspect(path_fragment)}', shell='#{inspect(shell)}'. Result: #{inspect(result)}"
    # )

    result
  end

  @doc false
  def exports(mod) do
    if Code.ensure_loaded?(mod) and function_exported?(mod, :__info__, 1) do
      mod.__info__(:macros) ++ (mod.__info__(:functions) -- [__info__: 1])
    else
      mod.module_info(:exports) -- [module_info: 0, module_info: 1]
    end
  end

  ## Formatting

  @doc false
  def format_expansion([], _) do
    no()
  end

  def format_expansion([uniq], hint) do
    case to_hint(uniq, hint) do
      "" -> yes("", to_entries(uniq))
      hint -> yes(hint, [])
    end
  end

  def format_expansion([first | _] = entries, hint) do
    binary = Enum.map(entries, & &1.name)
    length = byte_size(hint)
    prefix = :binary.longest_common_prefix(binary)

    if prefix in [0, length] do
      yes("", Enum.flat_map(entries, &to_entries/1))
    else
      yes(binary_part(first.name, prefix, length - prefix), [])
    end
  end

  def yes(hint, entries) do
    {:yes, String.to_charlist(hint), Enum.map(entries, &String.to_charlist/1)}
  end

  def no do
    {:no, '', []}
  end

  ## Ad-hoc conversions

  defp to_entries(%{kind: :function, name: name, arity: arity}) do
    ["#{name}/#{arity}"]
  end

  defp to_entries(%{kind: :sigil, name: name}) do
    ["~#{name} (sigil_#{name})"]
  end

  defp to_entries(%{kind: :keyword, name: name}) do
    ["#{name}:"]
  end

  defp to_entries(%{kind: _, name: name}) do
    [name]
  end

  # Add extra character only if pressing tab when done
  defp to_hint(%{kind: :module, name: hint}, hint) do
    "."
  end

  defp to_hint(%{kind: :map_key, name: hint, value_is_map: true}, hint) do
    "."
  end

  defp to_hint(%{kind: :file, name: hint}, hint) do
    "\""
  end

  # Add extra character whenever possible
  defp to_hint(%{kind: :dir, name: name}, hint) do
    format_hint(name, hint) <> "/"
  end

  defp to_hint(%{kind: :struct, name: name}, hint) do
    format_hint(name, hint) <> "{"
  end

  defp to_hint(%{kind: :keyword, name: name}, hint) do
    format_hint(name, hint) <> ": "
  end

  defp to_hint(%{kind: _, name: name}, hint) do
    format_hint(name, hint)
  end

  defp format_hint(name, hint) do
    hint_size = byte_size(hint)
    binary_part(name, hint_size, byte_size(name) - hint_size)
  end
end
