defmodule Mix.PublicKey do
  @moduledoc false

  @in_memory_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAslPz1mAfyAvRv8W8xOdv
  HQMbDJkDKfRhsL4JBGwGH7qw0xh+TbaUlNaM3pF+i8VUjS/4FeXjT/OAUEAHu5Y2
  rBVlx00QcH8Dpbyf+H73XiCs0MXnTSecqDgzx6i6NMi8knklHT7yHySHtuuPmPuN
  Po8QTKolCKftwPE/iNDeyZfwufd+hTCoCQdoTVcB01SElfNtvKRtoKbx35q80IPr
  rOcGsALmr58+bWqCTY/51kFeRxzrPJ5LdcLU/AebyWddD4IUfPDxk16jTiCagMWA
  JPSwo8NUrWDIBbD+rEUp06y0ek276rG5Tzm/3Bma56RN/u6nAqBTBE8F2Hu2QBKj
  lQIDAQAB
  -----END PUBLIC KEY-----
  """

  @doc """
  Returns the file system path for public keys.
  """
  def public_keys_path, do: Path.join(Mix.Utils.mix_home(), "public_keys")

  @doc """
  Returns all public keys as a list.
  """
  def public_keys do
    path = public_keys_path()

    [{"in-memory public key for Elixir v#{System.version()}", @in_memory_key}] ++
      case File.ls(path) do
        {:ok, keys} -> Enum.map(keys, &{&1, File.read!(Path.join(path, &1))})
        {:error, _} -> []
      end
  end

  @doc """
  Decodes a public key and raises if the key is invalid.
  """
  def decode!(id, key) do
    [rsa_public_key] = :public_key.pem_decode(key)
    :public_key.pem_entry_decode(rsa_public_key)
  rescue
    _ ->
      Mix.raise("""
      Could not decode public key: #{id}. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format
      """)
  end

  @doc """
  Verifies the given binary has the proper signature using the system public keys.
  """
  def verify(binary, hash, signature) do
    Enum.any?(public_keys(), fn {id, key} ->
      :public_key.verify(binary, hash, signature, decode!(id, key))
    end)
  end
end
