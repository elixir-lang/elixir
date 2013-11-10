defmodule Mix.Server do
  @moduledoc false
  use GenServer.Behaviour

  defrecord Config, tasks: :ordsets.new, mixfile: [],
    shell: Mix.Shell.IO, scm: :ordsets.new, env: nil

  def start_link(env) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, env, [])
  end

  def call(arg) do
    :gen_server.call(__MODULE__, arg, 30_000)
  end

  def cast(arg) do
    :gen_server.cast(__MODULE__, arg)
  end

  ## Callbacks

  def init(env) do
    { :ok, Config[env: env] }
  end

  def handle_call(:tasks, _from, config) do
    { :reply, config.tasks, config }
  end

  def handle_call(:shell, _from, config) do
    { :reply, config.shell, config }
  end

  def handle_call(:scm, _from, config) do
    { :reply, config.scm, config }
  end

  def handle_call(:env, _from, config) do
    { :reply, config.env, config }
  end

  def handle_call(:clear_tasks, _from, config) do
    { :reply, config.tasks, config.tasks(:ordsets.new) }
  end

  def handle_call({ :has_task?, task, app }, _from, config) do
    { :reply, :ordsets.is_element({task, app}, config.tasks), config }
  end

  def handle_call({ :mixfile_cache, app }, _from, config) do
    { :reply, config.mixfile[app], config }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast({ :shell, name }, config) do
    { :noreply, config.shell(name) }
  end

  def handle_cast({ :env, env }, config) when is_atom(env) do
    { :noreply, config.env(env) }
  end

  def handle_cast({ :add_task, task, app }, config) do
    { :noreply, config.update_tasks &:ordsets.add_element({task, app}, &1) }
  end

  def handle_cast({ :delete_task, task, app }, config) do
    { :noreply, config.update_tasks &:ordsets.del_element({task, app}, &1) }
  end

  def handle_cast({ :delete_task, task }, config) do
    { :noreply, config.update_tasks &:ordsets.filter(fn {t, _} -> t != task end, &1) }
  end

  def handle_cast({ :add_scm, mod }, config) do
    { :noreply, config.update_scm &:ordsets.add_element(mod, &1) }
  end

  def handle_cast({ :mixfile_cache, app, new }, config) do
    { :noreply, config.update_mixfile(&Keyword.merge(&1, [{ app, new }])) }
  end

  def handle_cast(:clear_mixfile_cache, config) do
    { :noreply, config.mixfile([]) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end
