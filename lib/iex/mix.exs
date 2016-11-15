defmodule IEx.Mixfile do
  use Mix.Project

  def project do
    [app: :iex,
     version: System.version,
     build_per_environment: false]
  end

  def application do
    [registered: [IEx.Supervisor, IEx.Config],
     mod: {IEx.App, []},
     env: [
      autocomplete_server: IEx.Server,
      colors: [],
      inspect: [pretty: true],
      history_size: 20,
      default_prompt: "%prefix(%counter)>",
      alive_prompt: "%prefix(%node)%counter>"]]
  end
end
