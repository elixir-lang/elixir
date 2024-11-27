defprotocol JSON.Encoder do
  @moduledoc """
  A protocol for custom JSON encoding of data structures.

  If you have a struct, you can derive the implementation of this protocol
  by specifying which fields should be encoded to JSON:

        @derive {JSON.Encoder, only: [....]}
        defstruct ...

  It is also possible to encode all fields or skip some fields via the
  `:except` option, although this should be used carefully to avoid
  accidentally leaking private information when new fields are added:

      @derive JSON.Encoder
      defstruct ...

  Finally, if you don't own the struct you want to encode to JSON,
  you may use Protocol.derive/3 placed outside of any module:

      Protocol.derive(JSON.Encoder, NameOfTheStruct, only: [...])
      Protocol.derive(JSON.Encoder, NameOfTheStruct)
  """

  @undefined_impl_description """
  the protocol must be explicitly implemented.

  If you have a struct, you can derive the implementation specifying \
  which fields should be encoded to JSON:

      @derive {JSON.Encoder, only: [....]}
      defstruct ...

  It is also possible to encode all fields, although this should be \
  used carefully to avoid accidentally leaking private information \
  when new fields are added:

      @derive JSON.Encoder
      defstruct ...

  Finally, if you don't own the struct you want to encode to JSON, \
  you may use Protocol.derive/3 placed outside of any module:

      Protocol.derive(JSON.Encoder, NameOfTheStruct, only: [...])
      Protocol.derive(JSON.Encoder, NameOfTheStruct)\
  """

  @impl true
  defmacro __deriving__(module, opts) do
    fields = module |> Macro.struct_info!(__CALLER__) |> Enum.map(& &1.field)
    fields = fields_to_encode(fields, opts)
    vars = Macro.generate_arguments(length(fields), __MODULE__)
    kv = Enum.zip(fields, vars)

    {io, _prefix} =
      Enum.flat_map_reduce(kv, ?{, fn {field, value}, prefix ->
        key = IO.iodata_to_binary([prefix, :elixir_json.encode_binary(Atom.to_string(field)), ?:])
        {[key, quote(do: encoder.(unquote(value), encoder))], ?,}
      end)

    io = if io == [], do: "{}", else: io ++ [?}]

    quote do
      defimpl JSON.Encoder, for: unquote(module) do
        def encode(%{unquote_splicing(kv)}, encoder) do
          unquote(io)
        end
      end
    end
  end

  defp fields_to_encode(fields, opts) do
    cond do
      only = Keyword.get(opts, :only) ->
        case only -- fields do
          [] ->
            only

          error_keys ->
            raise ArgumentError,
                  "unknown struct fields #{inspect(error_keys)} specified in :only. Expected one of: " <>
                    "#{inspect(fields -- [:__struct__])}"
        end

      except = Keyword.get(opts, :except) ->
        case except -- fields do
          [] ->
            fields -- [:__struct__ | except]

          error_keys ->
            raise ArgumentError,
                  "unknown struct fields #{inspect(error_keys)} specified in :except. Expected one of: " <>
                    "#{inspect(fields -- [:__struct__])}"
        end

      true ->
        fields -- [:__struct__]
    end
  end

  @doc """
  A function invoked to encode the given term.
  """
  def encode(term, encoder)
end

defimpl JSON.Encoder, for: Atom do
  def encode(value, encoder) do
    case value do
      nil -> "null"
      true -> "true"
      false -> "false"
      _ -> encoder.(Atom.to_string(value), encoder)
    end
  end
end

defimpl JSON.Encoder, for: BitString do
  def encode(value, _encoder) do
    :elixir_json.encode_binary(value)
  end
end

defimpl JSON.Encoder, for: List do
  def encode(value, encoder) do
    :elixir_json.encode_list(value, encoder)
  end
end

defimpl JSON.Encoder, for: Integer do
  def encode(value, _encoder) do
    :elixir_json.encode_integer(value)
  end
end

defimpl JSON.Encoder, for: Float do
  def encode(value, _encoder) do
    :elixir_json.encode_float(value)
  end
end

defimpl JSON.Encoder, for: Map do
  def encode(value, encoder) do
    :elixir_json.encode_map(value, encoder)
  end
end

defmodule JSON do
  @moduledoc ~S"""
  JSON encoding and decoding.

  Both encoder and decoder fully conform to [RFC 8259](https://tools.ietf.org/html/rfc8259) and
  [ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/)
  standards.

  ## Encoding

  Elixir built-in data structures are encoded to JSON as follows:

  | **Elixir**             | **JSON** |
  |------------------------|----------|
  | `integer() \| float()` | Number   |
  | `true \| false `       | Boolean  |
  | `nil`                  | Null     |
  | `binary()`             | String   |
  | `atom()`               | String   |
  | `list()`               | Array    |
  | `%{binary() => _}`     | Object   |
  | `%{atom() => _}`       | Object   |
  | `%{integer() => _}`    | Object   |

  You may also implement the `JSON.Encoder` protocol for custom data structures.

  ## Decoding

  Elixir built-in data structures are decoded from JSON as follows:

  | **JSON** | **Elixir**             |
  |----------|------------------------|
  | Number   | `integer() \| float()` |
  | Boolean  | `true \| false`        |
  | Null     | `nil`                  |
  | String   | `binary()`             |
  | Object   | `%{binary() => _}`     |

  """

  @moduledoc since: "1.18.0"

  @doc ~S"""
  Encodes the given term to JSON as a binary.

  The second argument is a function that is recursively
  invoked to encode a term.

  ## Examples

      iex> JSON.encode([123, "string", %{key: "value"}])
      "[123,\"string\",{\"key\":\"value\"}]"

  """
  def encode(term, encoder \\ &encode_value/2) do
    IO.iodata_to_binary(encoder.(term, encoder))
  end

  @doc ~S"""
  Encodes the given term to JSON as an iodata.

  This is the most efficient format if the JSON is going to be
  used for IO purposes.

  The second argument is a function that is recursively
  invoked to encode a term.

  ## Examples

      iex> data = JSON.encode_to_iodata([123, "string", %{key: "value"}])
      iex> IO.iodata_to_binary(data)
      "[123,\"string\",{\"key\":\"value\"}]"

  """
  def encode_to_iodata(term, encoder \\ &encode_value/2) do
    encoder.(term, encoder)
  end

  @doc """
  A shortcut for `encode/2` used for compatibility purposes.

  If you are targetting the `JSON` module directly, do not use
  this function, use `JSON.encode/2` instead. This function will
  be deprecated in Elixir v1.22
  """
  @doc deprecated: "Use JSON.encode/2 instead"
  # TODO: Deprecate on Elixir v1.22
  def encode!(term, encoder \\ &encode_value/2) do
    encode(term, encoder)
  end

  @doc """
  A shortcut for `encode_to_iodata/2` used for compatibility purposes.

  If you are targetting the `JSON` module directly, do not use
  this function, use `JSON.encode_to_iodata/2` instead. This function will
  be deprecated in Elixir v1.22
  """
  @doc deprecated: "Use JSON.encode_to_iodata/2 instead"
  # TODO: Deprecate on Elixir v1.22
  def encode_to_iodata!(term, encoder \\ &encode_value/2) do
    encode_to_iodata(term, encoder)
  end

  @doc """
  This is the default function used to recursively encode each value.
  """
  def encode_value(value, encoder) when is_atom(value) do
    case value do
      nil -> "null"
      true -> "true"
      false -> "false"
      _ -> encoder.(Atom.to_string(value), encoder)
    end
  end

  def encode_value(value, _encoder) when is_binary(value),
    do: :elixir_json.encode_binary(value)

  def encode_value(value, _encoder) when is_integer(value),
    do: :elixir_json.encode_integer(value)

  def encode_value(value, _encoder) when is_float(value),
    do: :elixir_json.encode_float(value)

  def encode_value(value, encoder) when is_list(value),
    do: :elixir_json.encode_list(value, encoder)

  def encode_value(%{} = value, encoder) when not is_map_key(value, :__struct__),
    do: :elixir_json.encode_map(value, encoder)

  def encode_value(value, encoder),
    do: JSON.Encoder.encode(value, encoder)
end
