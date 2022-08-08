defmodule IEx.MixProject do
  use Mix.Project

  def project do
    [
      app: :iex,
      version: System.version(),
      build_per_environment: false
    ]
  end

  def application do
    [
      registered: [IEx.Broker, IEx.Config, IEx.Pry, IEx.Supervisor],
      mod: {IEx.App, []},
      env: [
        colors: [],
        inspect: [pretty: true],
        history_size: 20,
        default_prompt: "%prefix(%counter)>",
        alive_prompt: "%prefix(%node)%counter>",
        autocompletion: [IEx.Autocomplete.Path, IEx.Autocomplete.Code]
      ]
    ]
  end
end
