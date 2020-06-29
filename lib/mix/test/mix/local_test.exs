Code.require_file("../test_helper.exs", __DIR__)

defmodule Mix.LocalTest do
  use MixTest.Case

  # openssl rsa -in elixirest.pem -pubout > elixirest.pub
  @public_key """
  -----BEGIN PUBLIC KEY-----
  MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA37moKP1dGGLhsP3d8Fwv
  W25SoYZUY2K+Iq7A0OBV36Rnb8yW3BWjfh5YtmPvUCfYUbNCW2HTMMgBntkQ4YmN
  B9tHVZazl2uX9lGCfZZPFc/9umvKRojCPkMN81MfTxqnY0oaLHr6DB86RsWHB+ld
  782Xf+nd9q3LFdUl8SGlKX7uzfVWd4EWYNcL7aLeLSupZWeNg8uVmY3zua0EgIlQ
  XryalIOZb/R+pwprWZoftCl+20FGYi/mJpo/idFtXsR0sJKF4X0W3NORT9RIRbs9
  WdjiFi+eIP7Nm8KSF4pbaXCqSmVf9cgvUuGTxc9/P5GcIPAlkcsSrE5peLyUCk5f
  2QIDAQAB
  -----END PUBLIC KEY-----
  """

  # openssl genrsa -aes256 -out elixirtest.pem -passout stdin 2048
  @private_key """
  -----BEGIN RSA PRIVATE KEY-----
  Proc-Type: 4,ENCRYPTED
  DEK-Info: AES-256-CBC,48BA5153DA2F120ECE063B33C1204A49

  5gp3daNWujH7o9S/dJQEt9TYTRP0pPZtU55PlZrzWt52optr7XHW/ENOm84g5J70
  QCPELp12jfQsNiPwbVWXKy2zD3QlNiAelf65hqLWJTWli7XIXfdP46VXOu67OKf9
  Ziw4HQ+AdBEwFt20wJst77iy17sNlyxp5DhNDonnSizzIowgUAJkoNI5aBUU6D8X
  KTSIftZW35Z4SudkazdoHepEfItZTI8mB4rvfn71Q4oOBA1rAuUUmdPWoPBfUHDa
  hvIp2T2Q8zZYqm0+SjDxZUYOOreE7fuf5NSLhHHt7+jyWQmtaVxnOWms72G+9xT0
  NGmOEB0WEg1kBsUbYOXXwyCAZhNA6MaKCtgjQczRTK+geS1xNaFc9FDEk3ZjN4Z8
  PxrKQoqo+2aQGVcatZWCom80Dci3bIv7iZNA/y1rjfBn+MeitMOGscP7/CBrJAbI
  bh1mvCu0McSnqlN0a+EuCVfJQYFMzjibpRVzKAST0QeaxXd5QxHfcPFPBLOpiVWc
  NjHaZsHORyoJbUKGA4rgOiSB63mv7SDRA2mvxWpwV/+6MuwBah6t6CGoEsAr1Hbn
  1ySt5w27bw3QEf2KTiuxDubo8UrF0eYzP5A9MH8vRpSRZHg8T3SBVfPJ/pM16Lnn
  5BaMUdxDFJeet5HUYoke9Zm3udh2BvwGiKhzc9Pbw/EcsCcvChMimRTasqTaRf+S
  uIm0Un7o+7kTuvBo2y87j2urCEUzft5QqEynbkR7p3vZnwoLLj+supXh3V8ivW4s
  Z6ql+ukRcWd/ode+lbSiYfAJCLc1tCqJ3kTnMnADJBlL0TX7YnwBwWuwwPuZgeAv
  F6nnBE1SBQ1WK+bjSVzIqmNFqsZw34wgpnz2heX0q8msF5pzd6EIeA+uz86k8XYh
  4eVZYGXxa4Exodh/MqEpRuN1ytWDXvHULh0gml7xwZC3R50UD8uBNt5RGjXUkjXc
  V0atKuvgzVlsB4xbDhVP7EVYHBF02NfNOsvo7kh0Yl1IcT/42UaCGYuU1o9zotPv
  9b3SHz/HOmBVj2uCdR5XZ4EolP5Iv9vqIDt9DsuDpOyO+AFOww0FnJNCQ1Hmfb0T
  qBYPv994oSPYLCGR4a8i/xfmmV8KbAIVEgK3AMbz8RxKr3WBWXWnzQdr4+y4EG24
  hSnR52XQ42edv/fkqf9ez+fKNQ9i7PtlPE96Q21NeLMNKHh43X8hJFDh+oPz3Aio
  YSNMCZnoyRdrjBRCsVBpnyoLmuhWwG9RlcrEj3G0BxYPh/weaBOAKAHjSr28yuUj
  yIa8uddszC6XHSiVUgu7SGO8gQmq++eNdckjX/pEug5MjcWLUqaUg6+YLFWY6NLf
  uDPOYuivq7ErtKTvP2xl3TBEDKhdfqxA2+RFxbBDmKjffZnRkcknQsxhlzAdbg22
  Jwa2B1nrfjJpX5F+1Av2jHQGbIKMqZzv8fo1binMKpptFzokbWEOjcPCb3tPuomG
  ZRkW3qO2pdyYX2N7VXYG9tGi2HrN/oFrWnHPoYF23v85V8WxNkODOCpTz85e6R5v
  PVu+FCNFj5weEOTRhtEQyJo7mU5qIRwYeZvVxiC6W+XeFs95wdBE/Lvpg8yZ8D9d
  -----END RSA PRIVATE KEY-----
  """

  @csv """
  1.2.5,ABC,0.9.0
  1.2.3,DEF,1.0.0
  1.2.4,GHI,1.0.0
  """

  # openssl dgst -sha512 -sign elixirtest.pem hex-1.x.csv | openssl base64 > elixirtest.csv.signed
  @csv_signed """
  VRydmXOdEXQcKJu/SK/nKnE00T+s/T4mpXrYROMSXhD/s8ClvdimnGg61ie3YBS6
  LXOjlEhbtMHRM2rTOUvv4z7FcyzwvSxSjunlVi2g3c1pVOZ78MonnYhGb44tZw/q
  SOVmV+jJhc9EZFMIAAM3plMoyssyw2pMh7ZB/DxCQTIem3Qf0Ujzc2bYkLVlw7R+
  1Rn6dcYEgCzyldVkAUMaYBwieyweWALA+YVDCMudJJK2J7p1OnuoPSVV+N3OkB/Z
  T6Jj5ljD+54XnuxAMcgCoF9lpOwXscnw/Ma+8JqIoWo0jNFE3ji+8dGCUzQUdSe8
  llLXgJJE2tGpDhEXBA3idg==
  """

  # We don't actually use it but it exists for documentation purposes.
  _ = @private_key

  setup_all do
    File.mkdir_p!(Mix.PublicKey.public_keys_path())

    Mix.PublicKey.public_keys_path()
    |> Path.join("test_key.pub")
    |> File.write!(@public_key)
  end

  @tag :tmp_dir
  test "select correct versions from csv", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      File.write!("csv", @csv)
      File.write!("csv.signed", @csv_signed)

      assert {"1.0.0", "1.2.4", "GHI"} =
               Mix.Local.find_matching_versions_from_signed_csv!("name", "csv")
    end)
  end

  @tag :tmp_dir
  test "raise on bad signature", %{tmp_dir: tmp_dir} do
    File.cd!(tmp_dir, fn ->
      csv_signed = String.replace(@csv_signed, "VRy", "BAD")
      File.write!("csv", @csv)
      File.write!("csv.signed", csv_signed)

      assert_raise Mix.Error, fn ->
        Mix.Local.find_matching_versions_from_signed_csv!("name", "csv")
      end
    end)
  end
end
