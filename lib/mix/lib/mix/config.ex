defmodule Mix.Config do
  @moduledoc """
  Module for reading and merging app configurations.
  """

  @doc """
  Reads a configuration file.

  It returns the read configuration and a list of
  dependencies this configuration may have on.
  """
  def read(file) do
    config = Code.eval_file(file) |> elem(0)
    validate!(config)
    config
  end

  @doc """
  Validates a configuration.
  """
  def validate!(config) do
    if is_list(config) do
      Enum.all?(config, fn
        {app, value} when is_atom(app) ->
          if Keyword.keyword?(value) do
            true
          else
            raise ArgumentError, message:
              "expected config for app #{inspect app} to return keyword list, got: #{inspect value}"
          end
        _ ->
          false
      end)
    else
      raise ArgumentError, message:
        "expected config to return keyword list, got: #{inspect config}"
    end
  end

  @doc """
  Merges two configurations.

  The configuration of each application is merged together
  with the values in the second one having higher preference
  than the first in case of conflicts.

  ## Examples

      iex> Mix.Config.merge([app: [k: :v1]], [app: [k: :v2]])
      [app: [k: :v2]]

      iex> Mix.Config.merge([app1: []], [app2: []])
      [app1: [], app2: []]

  """
  def merge(config1, config2) do
    Keyword.merge(config1, config2, fn _, app1, app2 ->
      Keyword.merge(app1, app2)
    end)
  end

  @doc """
  Merges two configurations.

  The configuration of each application is merged together
  and a callback is invoked in case of conflicts receiving
  the app, the conflicting key and both values. It must return
  a value that will be used as part of the conflict resolution.

  ## Examples

      iex> Mix.Config.merge([app: [k: :v1]], [app: [k: :v2]],
      ...>   fn app, k, v1, v2 -> {app, k, v1, v2} end)
      [app: [k: {:app, :k, :v1, :v2}]]

  """
  def merge(config1, config2, callback) do
    Keyword.merge(config1, config2, fn app, app1, app2 ->
      Keyword.merge(app1, app2, fn k, v1, v2 ->
        if v1 == v2 do
          v1
        else
          callback.(app, k, v1, v2)
        end
      end)
    end)
  end
end
