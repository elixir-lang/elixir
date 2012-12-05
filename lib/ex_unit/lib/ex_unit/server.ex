defmodule ExUnit.Server do
  @moduledoc false

  use GenServer.Behaviour

  defrecord Config, options: [], async_cases: [], sync_cases: [], after_spawn: []

  def start_link(options) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
  end

  ## Before run API

  def add_async_case(name) do
    :gen_server.cast(__MODULE__, { :add_async_case, name })
  end

  def add_sync_case(name) do
    :gen_server.cast(__MODULE__, { :add_sync_case, name })
  end

  def add_after_spawn(fun) when is_function(fun) do
    :gen_server.cast(__MODULE__, { :add_after_spawn, fun })
  end

  def merge_options(options) do
    :gen_server.cast(__MODULE__, { :merge_options, options })
  end

  ## After run API

  def options do
    :gen_server.call(__MODULE__, :options)
  end

  def take_sync_cases do
    :gen_server.call(__MODULE__, :take_sync_cases)
  end

  def take_async_cases(count) when count > 0 do
    :gen_server.call(__MODULE__, { :take_async_cases, count })
  end

  def run_after_spawn do
    funs = :gen_server.call(__MODULE__, :after_spawn)
    lc fun inlist funs, do: fun.()
  end

  ## Callbacks

  def init(options) do
    { :ok, Config[options: options] }
  end

  def handle_call(:options, _from, config) do
    { :reply, config.options, config }
  end

  def handle_call(:after_spawn, _from, config) do
    { :reply, Enum.reverse(config.after_spawn), config }
  end

  def handle_call({:take_async_cases, count}, _from, config) do
    case config.async_cases do
      [] ->
        { :reply, nil, config }
      cases ->
        { response, remaining } = Enum.split(cases, count)
        { :reply, response, config.async_cases(remaining) }
    end
  end

  def handle_call(:take_sync_cases, _from, config) do
    case config.sync_cases do
      [h|t] -> { :reply, [h], config.sync_cases(t) }
      []    -> { :reply, nil, config }
    end
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast({:add_async_case, name}, config) do
    { :noreply, config.update_async_cases [name|&1] }
  end

  def handle_cast({:add_sync_case, name}, config) do
    { :noreply, config.update_sync_cases [name|&1] }
  end

  def handle_cast({:add_after_spawn, fun}, config) do
    { :noreply, config.update_after_spawn [fun|&1] }
  end

  def handle_cast({:merge_options, options}, config) do
    { :noreply, config.update_options(&1 /> Keyword.merge(options)) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
