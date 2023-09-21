defmodule Umbrella.MixProject do
  use Mix.Project

  def project do
    [apps_path: "apps"]
  end

  def application do
    [extra_applications: [:runtime_tools]]
  end
end
