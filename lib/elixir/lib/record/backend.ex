defmodule Record.Backend do
  # Callback functions invoked by defrecord, defrecordp and friends.
  @moduledoc false

  @doc """
  Splits a keywords list into fields and types.

  This logic is shared by records and structs.
  """
  def split_fields_and_types(tag, kv) when is_list(kv) do
    split_fields_and_types(tag, kv, [], [])
  end

  def split_fields_and_types(tag, other) do
    raise ArgumentError, message: "#{tag} fields must be a keyword list, got: #{Macro.to_string other}"
  end

  defp split_fields_and_types(tag, [{ field, { :::, _, [default, type] }}|t], fields, types) do
    split_fields_and_types(tag, t, [{ field, default }|fields], [{ field, type }|types])
  end

  defp split_fields_and_types(tag, [{ field, default }|t], fields, types) when is_atom(field) do
    split_fields_and_types(tag, t, [{ field, default }|fields], [{ field, quote(do: term) }|types])
  end

  defp split_fields_and_types(tag, [field|t], fields, types) when is_atom(field) do
    split_fields_and_types(tag, t, [{ field, nil }|fields], [{ field, quote(do: term) }|types])
  end

  defp split_fields_and_types(tag, [other|_], _fields, _types) do
    raise ArgumentError, message: "#{tag} fields must be atoms, got: #{Macro.to_string other}"
  end

  defp split_fields_and_types(_tag, [], fields, types) do
    { :lists.reverse(fields), :lists.reverse(types) }
  end
end
