defmodule Mix.PublicKey do
  @moduledoc false

  @in_memory_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA+ynw5jnGF5S+rdUoa2R9
  Z2iQMwN/oNpWre48XC/aQo4X9nWflzq63Mgmm/XJk3rV7oaNUu23gnP+KUwxkOSV
  6hsk9PEdE9cg/DjVB8p4332Lay5EQk96oeEm2igYk0Mj7TnaUWLZs8FqIFbX+zfb
  jAMhbz6tnjKHiVgchxDsc0gGOOMkIlY+Dwc/CHoE/c/vcGuclZyLXRt141gXmB7+
  KDG4caWTs7SHwKJNaYrgZ77fSSF4Fk+jbQF1479ZWiI/mdhMweJ+4i7v+IVh2vyz
  JAV/94hTL8xIK6grnjtxZytqPL5GMEs3vydbOMILDCqpvPnC/AhjL5LwqwpaVaRw
  jwIDAQAB
  -----END PUBLIC KEY-----
  """

  @doc """
  Returns the filesystem path for public keys.
  """
  def public_keys_path, do: Path.join(Mix.Utils.mix_home, "public_keys")

  @doc """
  Returns all public keys as a list.
  """
  def public_keys do
    path = public_keys_path()

    [{"in-memory public key for Elixir v#{System.version}", @in_memory_key}] ++
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
      Mix.raise """
      Could not decode public key: #{id}. The public key contents are shown below.

      #{key}

      Public keys must be valid and be in the PEM format.
      """
  end

  @doc """
  Verifies the given binary has the proper signature using the system public keys.
  """
  def verify(binary, hash, signature) do
    Enum.any? public_keys(), fn {id, key} ->
      :public_key.verify binary, hash, signature, decode!(id, key)
    end
  end
end
