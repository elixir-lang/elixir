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

  @doc """
  Callback invoked from record/0 and record/1 macros.
  """
  def access(atom, fields, args, caller) do
    cond do
      is_atom(args) ->
        index(atom, fields, args)
      Keyword.keyword?(args) ->
        create(atom, fields, args, caller)
      true ->
        raise ArgumentError,
          message: "expected arguments to be a compile time atom or keywords, got: #{Macro.to_string args}"
    end
  end

  @doc """
  Callback invoked from the record/2 macro.
  """
  def access(atom, fields, record, args, caller) do
    cond do
      is_atom(args) ->
        get(atom, fields, record, args)
      Keyword.keyword?(args) ->
        update(atom, fields, record, args, caller)
      true ->
        raise ArgumentError,
          message: "expected arguments to be a compile time atom or keywords, got: #{Macro.to_string args}"
    end
  end

  @doc """
  Gets the index of field.
  """
  def index(atom, fields, field) do
    if index = find_index(fields, field, 0) do
      index - 1 # Convert to Elixir index
    else
      raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect field}"
    end
  end

  @doc """
  Creates a new record with the given default fields and keyword values.
  """
  def create(atom, fields, keyword, caller) do
    in_match = caller.in_match?

    { match, remaining } =
      Enum.map_reduce(fields, keyword, fn({ field, default }, each_keyword) ->
        new_fields =
          case Keyword.has_key?(each_keyword, field) do
            true  -> Keyword.get(each_keyword, field)
            false ->
              case in_match do
                true  -> { :_, [], nil }
                false -> Macro.escape(default)
              end
          end

        { new_fields, Keyword.delete(each_keyword, field) }
      end)

    case remaining do
      [] ->
        { :{}, [], [atom|match] }
      _  ->
        keys = for { key, _ } <- remaining, do: key
        raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect hd(keys)}"
    end
  end

  @doc """
  Updates a record given by var with the given keyword.
  """
  def update(atom, fields, var, keyword, caller) do
    if caller.in_match? do
      raise ArgumentError, message: "cannot invoke update style macro inside match"
    end

    Enum.reduce keyword, var, fn({ key, value }, acc) ->
      index = find_index(fields, key, 0)
      if index do
        quote do
          :erlang.setelement(unquote(index), unquote(acc), unquote(value))
        end
      else
        raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect key}"
      end
    end
  end

  @doc """
  Gets a record key from the given var.
  """
  def get(atom, fields, var, key) do
    index = find_index(fields, key, 0)
    if index do
      quote do
        :erlang.element(unquote(index), unquote(var))
      end
    else
      raise ArgumentError, message: "record #{inspect atom} does not have the key: #{inspect key}"
    end
  end

  defp find_index([{ k, _ }|_], k, i), do: i + 2
  defp find_index([{ _, _ }|t], k, i), do: find_index(t, k, i + 1)
  defp find_index([], _k, _i), do: nil
end
