defmodule ProtocolCover.MixProject do
  use Mix.Project

  def project do
    [
      app: :protocol_cover,
      version: "0.0.1",
      test_pattern: "*_protocol_cover.exs"
    ]
  end
end
