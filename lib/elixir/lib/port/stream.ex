defmodule Port.Stream do
  @moduledoc """
  Defines a `Port.Stream` struct returned by `Port.stream!/3`.

  The following fields are public:

    * `name`     - the name tuple expected by `Port.open/2`
    * `settings` - the settings arguments expected by `Port.open/2`
    * `raw`      - if set to `true`, the stream of data is proxied from the port, with no modifications

  """

  defstruct name: nil, settings: [], raw: false

  @type t :: %__MODULE__{}

  @doc false
  def __build__(name, settings, raw) do
    settings = case :exit_status in settings do
      true  -> settings
      false -> [:exit_status | settings]
    end

    %Port.Stream{name: name, settings: settings, raw: raw}
  end

  defimpl Collectable do
    def into(%{name: name, settings: settings, raw: raw} = stream) do
      {:ok, server_pid} = Port.Server.start_link(name, settings, raw)
      into(server_pid, stream)
    end

    defp into(server_pid, stream) do
      fn
        :ok, {:cont, x} ->
          :ok = Port.Server.command(server_pid, x)
        :ok, :done ->
          :ok = Port.Server.close(server_pid)
          stream
        :ok, :halt ->
          :ok = Port.Server.close(server_pid)
      end
    end
  end

  defimpl Enumerable do
    def reduce(%{name: name, settings: settings, raw: raw}, acc, fun) do
      start_fun = fn ->
        {:ok, server_pid} = Port.Server.start_link(name, settings, raw)
        server_pid
      end

      next_fun = &Port.Server.fetch/1
      stop_fun = &Process.exit(&1, :normal)

      Stream.resource(start_fun, next_fun, stop_fun).(acc, fun)
    end

    def count(_stream) do
      {:error, __MODULE__}
    end

    def member?(_stream, _term) do
      {:error, __MODULE__}
    end
  end
end
