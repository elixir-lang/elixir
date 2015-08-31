defmodule Mix.PublicKey do
  @moduledoc false

  @pub_key """
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

  def verify(binary, hash, signature) do
    [rsa_public_key] = :public_key.pem_decode(@pub_key)
    rsa_public_key   = :public_key.pem_entry_decode(rsa_public_key)
    :public_key.verify binary, hash, signature, rsa_public_key
  end
end
