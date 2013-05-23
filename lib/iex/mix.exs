defmodule IEx.Mixfile do
  use Mix.Project

  def project do
    [app: :iex, version: System.version]
  end

  def application do
    [env: [
      after_spawn: [],
      inspect_opts: [limit: 50],
      colors: [enabled: true, eval_result: "yellow", error: "red", info: "yellow"],
      started: true
    ]]
  end
end
