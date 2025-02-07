# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team

defprotocol JSON.Encoder do
  @moduledoc """
  A protocol for custom JSON encoding of data structures.

  If you have a struct, you can derive the implementation of this protocol
  by specifying which fields should be encoded to JSON:

      @derive {JSON.Encoder, only: [....]}
      defstruct ...

  It is also possible to encode all fields or skip some fields via the
  `:except` option:

      @derive JSON.Encoder
      defstruct ...

  > #### Leaking Private Information {: .error}
  >
  > The `:except` approach should be used carefully to avoid
  > accidentally leaking private information when new fields are added.

  Finally, if you don't own the struct you want to encode to JSON,
  you may use `Protocol.derive/3` placed outside of any module:

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
  A function invoked to encode the given term to `t:iodata/0`.
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

defimpl JSON.Encoder, for: Duration do
  def encode(value, _encoder) do
    [?", Duration.to_iso8601(value), ?"]
  end
end

defimpl JSON.Encoder, for: Date do
  def encode(%{calendar: Calendar.ISO} = date, _encoder) do
    %{year: year, month: month, day: day} = date
    [?", Calendar.ISO.date_to_iodata(year, month, day), ?"]
  end

  def encode(value, _encoder) do
    [?", Date.to_iso8601(value), ?"]
  end
end

defimpl JSON.Encoder, for: Time do
  def encode(%{calendar: Calendar.ISO} = time, _encoder) do
    %{
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    } = time

    [?", Calendar.ISO.time_to_iodata(hour, minute, second, microsecond), ?"]
  end

  def encode(value, _encoder) do
    [?", Time.to_iso8601(value), ?"]
  end
end

defimpl JSON.Encoder, for: NaiveDateTime do
  def encode(%{calendar: Calendar.ISO} = naive_datetime, _encoder) do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond
    } = naive_datetime

    [
      ?",
      Calendar.ISO.date_to_iodata(year, month, day),
      ?T,
      Calendar.ISO.time_to_iodata(hour, minute, second, microsecond),
      ?"
    ]
  end

  def encode(value, _encoder) do
    [?", NaiveDateTime.to_iso8601(value), ?"]
  end
end

defimpl JSON.Encoder, for: DateTime do
  def encode(%{calendar: Calendar.ISO} = datetime, _encoder) do
    %{
      year: year,
      month: month,
      day: day,
      hour: hour,
      minute: minute,
      second: second,
      microsecond: microsecond,
      time_zone: time_zone,
      utc_offset: utc_offset,
      std_offset: std_offset
    } = datetime

    [
      ?",
      Calendar.ISO.date_to_iodata(year, month, day),
      ?T,
      Calendar.ISO.time_to_iodata(hour, minute, second, microsecond),
      Calendar.ISO.offset_to_iodata(utc_offset, std_offset, time_zone, :extended),
      ?"
    ]
  end

  def encode(value, _encoder) do
    [?", DateTime.to_iso8601(value), ?"]
  end
end

defmodule JSON.DecodeError do
  @moduledoc """
  The exception raised by `JSON.decode!/1`.
  """
  defexception [:message, :offset, :data]
end

defmodule JSON do
  @moduledoc ~S"""
  JSON encoding and decoding.

  Both encoder and decoder fully conform to [RFC 8259](https://tools.ietf.org/html/rfc8259) and
  [ECMA 404](https://ecma-international.org/publications-and-standards/standards/ecma-404/)
  standards.

  ## Encoding

  Elixir primitive types are encoded to JSON as follows:

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
  Some built-in data-structures already derive the `JSON.Encoder` protocol:

  | **Elixir**             | **JSON**        |
  |------------------------|-----------------|
  | `Date.t()`             | ISO 8601 string |
  | `Time.t()`             | ISO 8601 string |
  | `DateTime.t()`         | ISO 8601 string |
  | `NaiveDateTime.t()`    | ISO 8601 string |
  | `Duration.t()`         | ISO 8601 string |

  ## Decoding

  Elixir types are decoded from JSON as follows:

  | **JSON** | **Elixir**             |
  |----------|------------------------|
  | Number   | `integer() \| float()` |
  | Boolean  | `true \| false`        |
  | Null     | `nil`                  |
  | String   | `binary()`             |
  | Object   | `%{binary() => _}`     |

  """

  @moduledoc since: "1.18.0"

  @type encoder :: (term(), encoder() -> iodata())

  @type decode_error_reason ::
          {:unexpected_end, non_neg_integer()}
          | {:invalid_byte, non_neg_integer(), byte()}
          | {:unexpected_sequence, non_neg_integer(), binary()}

  @doc ~S"""
  Decodes the given JSON.

  Returns `{:ok, decoded}` or `{:error, reason}`.

  ## Examples

      iex> JSON.decode("[null,123,\"string\",{\"key\":\"value\"}]")
      {:ok, [nil, 123, "string", %{"key" => "value"}]}

  ## Error reasons

  The error tuple will have one of the following reasons.

    * `{:unexpected_end, offset}` if `binary` contains incomplete JSON value
    * `{:invalid_byte, offset, byte}` if `binary` contains unexpected byte or invalid UTF-8 byte
    * `{:unexpected_sequence, offset, bytes}` if `binary` contains invalid UTF-8 escape
  """
  @spec decode(binary()) :: {:ok, term()} | {:error, decode_error_reason()}
  def decode(binary) when is_binary(binary) do
    with {decoded, :ok, rest} <- decode(binary, :ok, []) do
      if rest == "" do
        {:ok, decoded}
      else
        {:error, {:invalid_byte, byte_size(binary) - byte_size(rest), :binary.at(rest, 0)}}
      end
    end
  end

  @doc ~S"""
  Decodes the given JSON with the given decoders.

  Returns `{decoded, acc, rest}` or `{:error, reason}`.
  See `decode/1` for the error reasons.

  ## Decoders

  All decoders are optional. If not provided, they will fall back to
  implementations used by the `decode/1` function:

    * for `array_start`: `fn _ -> [] end`
    * for `array_push`: `fn elem, acc -> [elem | acc] end`
    * for `array_finish`: `fn acc, old_acc -> {Enum.reverse(acc), old_acc} end`
    * for `object_start`: `fn _ -> [] end`
    * for `object_push`: `fn key, value, acc -> [{key, value} | acc] end`
    * for `object_finish`: `fn acc, old_acc -> {Map.new(acc), old_acc} end`
    * for `float`: `&String.to_float/1`
    * for `integer`: `&String.to_integer/1`
    * for `string`: `&Function.identity/1`
    * for `null`: the atom `nil`

  For streaming decoding, see Erlang's [`:json`](`:json`) module.
  """
  @spec decode(binary(), term(), keyword()) ::
          {term(), term(), binary()} | {:error, decode_error_reason()}
  def decode(binary, acc, decoders) when is_binary(binary) and is_list(decoders) do
    decoders = Keyword.put_new(decoders, :null, nil)

    try do
      :elixir_json.decode(binary, acc, Map.new(decoders))
    catch
      :error, :unexpected_end ->
        {:error, {:unexpected_end, byte_size(binary)}}

      :error, {:invalid_byte, byte} ->
        {:error, {:invalid_byte, offset(__STACKTRACE__), byte}}

      :error, {:unexpected_sequence, bytes} ->
        {:error, {:unexpected_sequence, offset(__STACKTRACE__), bytes}}
    end
  end

  defp offset(stacktrace) do
    with [{_, _, _, opts} | _] <- stacktrace,
         %{cause: %{position: position}} <- opts[:error_info] do
      position
    else
      _ -> 0
    end
  end

  @doc ~S"""
  Decodes the given JSON but raises an exception in case of errors.

  Returns the decoded content. See `decode/1` for possible errors.

  ## Examples

      iex> JSON.decode!("[null,123,\"string\",{\"key\":\"value\"}]")
      [nil, 123, "string", %{"key" => "value"}]
  """
  @spec decode!(binary()) :: term()
  def decode!(binary) when is_binary(binary) do
    case decode(binary) do
      {:ok, decoded} ->
        decoded

      {:error, {:unexpected_end, offset}} ->
        raise JSON.DecodeError,
          message: "unexpected end of JSON binary at position (byte offset) #{offset}",
          data: binary,
          offset: offset

      {:error, {:invalid_byte, offset, byte}} ->
        raise JSON.DecodeError,
          message: "invalid byte #{byte} at position (byte offset) #{offset}",
          data: binary,
          offset: offset

      {:error, {:unexpected_sequence, offset, bytes}} ->
        raise JSON.DecodeError,
          message: "unexpected sequence #{inspect(bytes)} at position (byte offset) #{offset}",
          data: binary,
          offset: offset
    end
  end

  @doc ~S"""
  Encodes the given term to JSON as a binary.

  The second argument is a function that is recursively
  invoked to encode a term.

  > #### IO and performance {: .tip}
  >
  > If you need to encode data to be sent over the network
  > or written to the filesystem, consider using the more
  > efficient `encode_to_iodata!/2`.

  ## Examples

      iex> JSON.encode!([123, "string", %{key: "value"}])
      "[123,\"string\",{\"key\":\"value\"}]"

  """
  @spec encode!(term(), encoder()) :: binary()
  def encode!(term, encoder \\ &protocol_encode/2) do
    IO.iodata_to_binary(encoder.(term, encoder))
  end

  @doc ~S"""
  Encodes the given term to JSON as an iodata.

  This is the most efficient format if the JSON is going to be
  used for IO purposes.

  The second argument is a function that is recursively
  invoked to encode a term.

  ## Examples

      iex> data = JSON.encode_to_iodata!([123, "string", %{key: "value"}])
      iex> IO.iodata_to_binary(data)
      "[123,\"string\",{\"key\":\"value\"}]"

  """
  @spec encode_to_iodata!(term(), encoder()) :: iodata()
  def encode_to_iodata!(term, encoder \\ &protocol_encode/2) do
    encoder.(term, encoder)
  end

  @doc """
  This is the default encode implementation passed to `encode!/1`.

  This function is most typically passed as second argument to
  `encode!/2` and `encode_to_iodata!/2`. The default implementation
  is an optimized dispatch to the `JSON.Encoder` protocol.
  """
  @spec protocol_encode(term(), encoder()) :: iodata()
  def protocol_encode(value, encoder) when is_atom(value) do
    case value do
      nil -> "null"
      true -> "true"
      false -> "false"
      _ -> encoder.(Atom.to_string(value), encoder)
    end
  end

  def protocol_encode(value, _encoder) when is_binary(value),
    do: :elixir_json.encode_binary(value)

  def protocol_encode(value, _encoder) when is_integer(value),
    do: :elixir_json.encode_integer(value)

  def protocol_encode(value, _encoder) when is_float(value),
    do: :elixir_json.encode_float(value)

  def protocol_encode(value, encoder) when is_list(value),
    do: :elixir_json.encode_list(value, encoder)

  def protocol_encode(%{} = value, encoder) when not is_map_key(value, :__struct__),
    do: :elixir_json.encode_map(value, encoder)

  def protocol_encode(value, encoder),
    do: JSON.Encoder.encode(value, encoder)
end
