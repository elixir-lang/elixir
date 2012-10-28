defmodule Mix.Server do
  @moduledoc false

  alias :ordsets, as: Ordset
  use GenServer.Behaviour

  defrecord Config, tasks: Ordset.new, projects: [], mixfile: [],
    shell: Mix.Shell.IO, scm: Ordset.new, env: nil, post_config: []

  def start_link(env) do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, env, [])
  end

  def call(arg) do
    :gen_server.call(__MODULE__, arg)
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

  def handle_call(:projects, _from, config) do
    { :reply, config.projects, config }
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
    { :reply, config.tasks, config.tasks(Ordset.new) }
  end

  def handle_call({ :has_task?, task }, _from, config) do
    { :reply, Ordset.is_element(task, config.tasks), config }
  end

  def handle_call(:pop_project, _from, config) do
    case config.projects do
      [{ project, _ }|tail] ->
        { :reply, project, config.projects(tail) }
      _ ->
        { :reply, nil, config }
    end
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

  def handle_cast({ :set_tasks, tasks }, config) do
    { :noreply, config.tasks(tasks) }
  end

  def handle_cast({ :add_task, name }, config) do
    { :noreply, config.update_tasks :ordsets.add_element(name, &1) }
  end

  def handle_cast({ :delete_task, name }, config) do
    { :noreply, config.update_tasks :ordsets.del_element(name, &1) }
  end

  def handle_cast({ :push_project, name, project }, config) do
    project = Keyword.merge(project, config.post_config)
    { :noreply, config.post_config([]).prepend_projects [ { name, project } ] }
  end

  def handle_cast({ :post_config, value }, config) do
    { :noreply, config.merge_post_config value }
  end

  def handle_cast({ :add_scm, mod }, config) do
    { :noreply, config.update_scm :ordsets.add_element(mod, &1) }
  end

  def handle_cast({ :mixfile_cache, app, new }, config) do
    { :noreply, config.merge_mixfile([{ app, new }]) }
  end

  def handle_cast(:clear_mixfile_cache, config) do
    { :noreply, config.mixfile([]) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end