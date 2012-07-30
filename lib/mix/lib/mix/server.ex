defmodule Mix.Server do
  @moduledoc false

  alias :ordsets, as: Ordset
  use GenServer.Behavior

  defrecord Config, tasks: Ordset.new, projects: [], shell: Mix.Shell, scm: Ordset.new

  def start_link do
    :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, [], [])
  end

  def call(arg) do
    :gen_server.call(__MODULE__, arg)
  end

  def cast(arg) do
    :gen_server.cast(__MODULE__, arg)
  end

  ## Callbacks

  def init(_args) do
    { :ok, Config.new }
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

  def handle_call(:clear_tasks, _from, config) do
    { :reply, config.tasks, config.tasks(Ordset.new) }
  end

  def handle_call({ :has_task?, task }, _from, config) do
    { :reply, Ordset.is_element(task, config.tasks), config }
  end

  def handle_call(request, from, config) do
    super(request, from, config)
  end

  def handle_cast({ :shell, name }, config) do
    { :noreply, config.shell(name) }
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

  def handle_cast({ :push_project, name }, config) do
    { :noreply, config.prepend_projects [name] }
  end

  def handle_cast(:pop_project, config) do
    { :noreply, config.update_projects tl(&1) }
  end

  def handle_cast({ :add_scm, mod }, config) do
    { :noreply, config.update_scm :ordsets.add_element(mod, &1) }
  end

  def handle_cast(request, config) do
    super(request, config)
  end
end