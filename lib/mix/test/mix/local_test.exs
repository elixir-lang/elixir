Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.LocalTest do
  use MixTest.Case

  # openssl rsa -in elixirtest.pem -pubout > elixirtest.pub
  @public_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAkIaIn8uhwSVp9d+aV+GL
  tFjMt0TylxDu6x5jwjDvXCD3Of6VAriqcdXDJAqGPN1C4HNTxfihYpLZB+qXCNQM
  oEa/I1h6OB5CXb8zIOl2Yriforr9LS+EkP+1xVmz7w7lNAagD0rJyJrTDbYDV+2Y
  EnRq84FpZc/+7z6ojc3RMh9x/5t9jDE4Ft3NCujCmGsy2AcBlMpAQkcNveyplNmu
  fZH2vId8h8t6rABwEZgSazHObkcHwE3bndynFO7zXvBu6ebNUDuU3DrDJ1Iepy9I
  jEuZ6h13j7hL5l+j+9bGRQtasWgwvCkCGD+/2Lz4Ehox58voDCzAK09rsCwQ6gAT
  CQIDAQAB
  -----END PUBLIC KEY-----
  """

  # openssl genrsa -aes256 -out elixirtest.pem -passout pass:secret12 2048
  @private_key """
  -----BEGIN ENCRYPTED PRIVATE KEY-----
  MIIFNTBfBgkqhkiG9w0BBQ0wUjAxBgkqhkiG9w0BBQwwJAQQjNQQlREZJuQveXPa
  0ACpEwICCAAwDAYIKoZIhvcNAgkFADAdBglghkgBZQMEASoEEFoBWTpEyO8y6xdG
  fuud2AIEggTQDQRXrxBpj1yIbnLcP7i43Gat2tEM+XXkvqbzwL+CdjfEzaqiBGwc
  FEyWnJj5hENWtNq/AAwzcEALa3QdLJ2W+tX/hc5r1vILpy2oW0vPKfv4yadNyPWq
  UzolKKU3MbREP4KXsuAVYepolRWAcWSZKozYBm9HUPwsSyM6E9K0aWmNr/YBbZPE
  UEA949Pq6buQvwT2Gj/6FQlutEvJMuY8woM2I/RKSVHeSoB8FWJGSNd7ZY2DN7DG
  o6kmhK0OVjxx0SYQwUm++4gh3yFRX6vnTf8ZADOwCOJE/hjMfqivNelJHGojYAD2
  0c3zdo0HGvVghg2bzUxhc0HJqUFiUDNW1W3AVBrUJiQgHQb/ENYaMrd6klrslHDV
  cnPkz+dZAq/WC3KFqufkSmHxU97dDs9G6hKdm80JVEdPG4aMJvHzVL6SvS+g/e7r
  IJGZCFGch9SbikmESKegIfyLJYQrfjuLdXWPHx9J+LAmtRTA1xPwLH/aqCr9ahBB
  JCl4RaezF0BH2GYLAV+Ailpy2jqsC3v9RW8vpQmU227H8ERFu9ZcWhZITbRhbjRB
  oh3ERnOmvU3VWIXQ0j1cm9Qjl52y8HZJxef2dYsW8nmTSRj32gi9UnEsyOFL9FB7
  KcFDq6o+go4nIfsmaAEOF1Hh01JZkbuX1SgmHvGRbNksTuljP/XZn7Wzz7fTcVZR
  NsTgMbBl1+ctDHUBT7YzsHeEx8tZvLPDP8lO23aEnmRb6bHD4hVorUEM/kOpSm6g
  DNP0buYGEAu4rZrp+5s46Y/RVAIhU4+663jqzmbmwOwEcuOJJCJn8eUGVweAgWX3
  1PQTfpkk7RLxxuLeMHxEIynxyZNpSgfX0VGKe5UtplwcTQL8VgeZHayDxnhFwtSm
  lBfPIM/i4kj7RonuehyPjuBA6n1CyqRVqc49c/3kVMgovqvbUQxQ7TtSfzfkhVyV
  yRnRxcek+6Gctoahas5DPQl7y8jnwgg5qvUjYo6GIwBoxLUzIw+0MBn7gdMYHNmh
  Xd9XZpRilAMJF5ynr2QyaE95XaQ3yK62FPgmIDdzfnN7es169O4SiXsgUyaSWsx9
  VxOHP4r0j6epWLH4W11MQEj4Bb8wG//pCD5Kz2xvwhWaKjkqzuqGDaXSOTKV0FQM
  NG5GbNNELNCy/ocvlsaC+acWcOfsx4f6yovCGVTpTr+wjbDqd1kEBoRTuyTPY+6P
  0OU1Vf2GpMA2NDw0ZUOAllGVLrgD+326bu7ipePgiT8SiPNNsagMJNCSk5d3C4+K
  bc0hakF/MIjPs3a+5/2mwtxk3QYYCIHITN8SIj+kFd7WMXALyClgWt3tcKwLTgMa
  BSatAvwsoi2kiBu2A49msvka7YD8fENhtUl8vU12ZZdTih1chyPDifgfRgOfkZFW
  a1XiLKjnWqeBqvJ3hBKuXweN6P7nvZidBYSESl3yrM00pA97Eta2in7ok80MRYH3
  bsqzC636h43Mu8PbTKGpK66j1ts6uY9YONAGfGCcfj/wyBWuqNViv7vK38th8wj8
  AG4rKyIlKxbFwo4Oh3jZTbfVF1Azl8pr0rK2P3W/EhfvWIrZVfB4Upy0wL6MjysE
  RZ/57N2wFq4KBb9x9hCMFxignWmgyzPK+1J0iPRkSmpn9P/SzywDBIg=
  -----END ENCRYPTED PRIVATE KEY-----
  """

  @csv """
  1.2.5,ABC,0.9.0,25
  1.2.5,ABC,0.9.0,26
  1.2.3,DEF,1.0.0,25
  1.2.3,DEF,1.0.0,26
  1.2.4,GHI,1.0.0,25
  1.2.4,GHI,1.0.0,26
  """

  # openssl dgst -sha512 -sign elixirtest.pem hex-1.x.csv | openssl base64 > elixirtest.csv.signed
  @csv_signed """
  CVkhTiuCAfooYPhjyynDq40QhmDwLAEJvpwYytPCf6mpLXVrLXo/d/A2L8iBRJVx
  uk4PNVksLRZ1ChBzGFvEqaFjrH+ndQAYLbwqcaMIn743YNUjGNVfTZkU47nBybtJ
  BwDSBaAsow0Iitsl+UkDN/QvVoOLiX/x2cpnwCMrCgbTMroTzhH07vfLo3uCf8iY
  cncImd07ffCewt77AsVPpKgJNOLzn+EBnvh4LbGWQya8EkgyQKuMBuNU86MYtFiW
  NVpR7vbvqgWpEyr1XeknxKkhzYpna3+irXdMxGZw65WvFNWGJKnpuBTNHnHL+wX8
  oNQLUfakH8/VMV/8v6Irbg==
  """

  # We don't actually use it but it exists for documentation purposes.
  _ = @private_key

  setup_all do
    File.mkdir_p!(Mix.Local.public_keys_path())

    Mix.Local.public_keys_path()
    |> Path.join("test_key.pub")
    |> File.write!(@public_key)
  end

  @tag :tmp_dir
  test "select correct versions from csv", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("csv", @csv)
      File.write!("csv.signed", @csv_signed)

      assert {"1.0.0", "1.2.4", "GHI", otp_release} =
               Mix.Local.find_matching_versions_from_signed_csv!("name", nil, "csv")

      assert otp_release <= System.otp_release()
    end)
  end

  @tag :tmp_dir
  test "select specific version from csv", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("csv", @csv)
      File.write!("csv.signed", @csv_signed)

      assert {"0.9.0", "1.2.5", "ABC", otp_release} =
               Mix.Local.find_matching_versions_from_signed_csv!("name", "1.2.5", "csv")

      assert otp_release <= System.otp_release()

      assert {"1.0.0", "1.2.3", "DEF", otp_release} =
               Mix.Local.find_matching_versions_from_signed_csv!("name", "1.2.3", "csv")

      assert otp_release <= System.otp_release()

      assert_raise Mix.Error, "Could not find a version of name matching: 1.3.0", fn ->
        Mix.Local.find_matching_versions_from_signed_csv!("name", "1.3.0", "csv")
      end
    end)
  end

  @tag :tmp_dir
  test "raise on bad signature", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      <<_, _, _>> <> rest = @csv_signed
      csv_signed = "BAD" <> rest

      File.write!("csv", @csv)
      File.write!("csv.signed", csv_signed)

      assert_raise Mix.Error, fn ->
        Mix.Local.find_matching_versions_from_signed_csv!("name", nil, "csv")
      end
    end)
  end
end
