# Heavily inspired by esupervisor (https://github.com/spawngrid/esupervisor)
defmodule Supervisor do
  @doc """
  Supervisor behavior wrapper

  Example:

  defmodule MySup do  
     use Supervisor.Behavior
     refer Supervisor, as: S
     def init(_) do
         S.OneForOne.new(children: [S.Worker.new(id: SomeModule)])
     end
  end
  """
  defrecord Child, id: nil, start_func: nil, restart: :permanent,
                   shutdown: 5000, type: nil, modules: []

  defrecord Worker, id: nil, start_func: nil, restart: :permanent,
                    shutdown: 5000, modules: []

  defrecord Sup, id: nil, registered: false, restart_strategy: :one_for_one,
                        max_restarts: {1,60}, children: [], restart: :permanent,
                        shutdown: :infinity

  defrecord OneForOne, id: nil, registered: false,
                        max_restarts: {1,60}, children: [], restart: :permanent,
                        shutdown: :infinity

  defrecord OneForAll, id: nil, registered: false,
                        max_restarts: {1,60}, children: [], restart: :permanent,
                        shutdown: :infinity
  defrecord RestForOne, id: nil, registered: false,
                        max_restarts: {1,60}, children: [], restart: :permanent,
                        shutdown: :infinity
  defrecord SimpleOneForOne, id: nil, registered: false,
                        max_restarts: {1,60}, children: [], restart: :permanent,
                        shutdown: :infinity

  defprotocol Strategy do
    def to_supervisor(record)
    def as_child(record)
  end

  defimpl Strategy, for: List do
    def to_supervisor([]), do: []
    def to_supervisor([h|t]), do: [Supervisor.Strategy.to_supervisor(h)|to_supervisor(t)]
    def as_child([]), do: []
    def as_child([h|t]), do: [Supervisor.Strategy.as_child(h)|as_child(t)]

  end

  defimpl Strategy, for: Worker do
    refer Worker, as: S
    def to_supervisor(S[]=s) do
        Supervisor.Strategy.to_supervisor(as_child(s))
    end
    def as_child(S[id: id, start_func: start_func,
                        restart: restart, shutdown: shutdown,
                        modules: modules]) do
        Child.new(type: :worker,
                  id: id,
                  start_func: start_func,
                  restart: restart,
                  shutdown: shutdown,
                  modules: modules)
    end
  end

  defimpl Strategy, for: Child do
    refer Child, as: S
    def to_supervisor(S[id: id, start_func: nil]=s) do
        to_supervisor(s.start_func({id, :start_link, []}))
    end
    def to_supervisor(S[id: id, modules: []]=s) do
        to_supervisor(s.modules([id]))
    end
    def to_supervisor(S[id: id, start_func: start_func,
                        restart: restart, shutdown: shutdown,
                        modules: modules, type: type]) do
        child = {id, start_func, restart, shutdown, type, modules}
        :ok = :supervisor.check_childspecs([child])
        child
    end
    def as_child(s), do: s
  end

  defimpl Strategy, for: OneForOne do
    refer OneForOne, as: S
    def to_supervisor(S[id: id, max_restarts: max_rt, children: children]) do
        Supervisor.Strategy.to_supervisor(Sup.new(id: id, 
                                                  restart_strategy: :one_for_one,
                                                  max_restarts: max_rt,
                                                  children: children))
    end
    def as_child(S[id: id, registered: reg, restart: restart,
                   shutdown: shutdown]=s) do
        Child.new(id: id, type: :supervisor,
                  start_func: {__MODULE__, :start_sup, [reg, s]},
                  restart: restart,
                  shutdown: shutdown,
                  modules: [__MODULE__])
    end
  end

  defimpl Strategy, for: OneForAll do
    refer OneForAll, as: S
    def to_supervisor(S[id: id, max_restarts: max_rt, children: children]) do
        Supervisor.Strategy.to_supervisor(Sup.new(id: id, 
                                                  restart_strategy: :one_for_all,
                                                  max_restarts: max_rt,
                                                  children: children))
    end
    def as_child(S[id: id, registered: reg, restart: restart,
                   shutdown: shutdown]=s) do
        Child.new(id: id, type: :supervisor,
                  start_func: {__MODULE__, :start_sup, [reg, s]},
                  restart: restart,
                  shutdown: shutdown,
                  modules: [__MODULE__])
    end
  end

  defimpl Strategy, for: RestForOne do
    refer RestForOne, as: S
    def to_supervisor(S[id: id, max_restarts: max_rt, children: children]) do
        Supervisor.Strategy.to_supervisor(Sup.new(id: id, 
                                                  restart_strategy: :rest_for_one,
                                                  max_restarts: max_rt,
                                                  children: children))
    end
    def as_child(S[id: id, registered: reg, restart: restart,
                   shutdown: shutdown]=s) do
        Child.new(id: id, type: :supervisor,
                  start_func: {__MODULE__, :start_sup, [reg, s]},
                  restart: restart,
                  shutdown: shutdown,
                  modules: [__MODULE__])
    end
  end

  defimpl Strategy, for: SimpleOneForOne do
    refer SimpleOneForOne, as: S
    def to_supervisor(S[id: id, max_restarts: max_rt, children: children]) do
        Supervisor.Strategy.to_supervisor(Sup.new(id: id, 
                                                  restart_strategy: :simple_one_for_one,
                                                  max_restarts: max_rt,
                                                  children: children))
    end
    def as_child(S[id: id, registered: reg, restart: restart,
                   shutdown: shutdown]=s) do
        Child.new(id: id, type: :supervisor,
                  start_func: {__MODULE__, :start_sup, [reg, s]},
                  restart: restart,
                  shutdown: shutdown,
                  modules: [__MODULE__])
    end
  end

  defimpl Strategy, for: Sup do
    refer Sup, as: S
    def to_supervisor(S[restart_strategy: restart,
                        max_restarts: {max_r, max_t}, children: children]) do
        {{restart, max_r, max_t}, Supervisor.Strategy.to_supervisor(Supervisor.Strategy.as_child(children))}
    end
    def as_child(S[id: id, registered: reg, restart: restart,
                   shutdown: shutdown]=s) do
        Child.new(id: id, type: :supervisor,
                  start_func: {__MODULE__, :start_sup, [reg, s]},
                  restart: restart,
                  shutdown: shutdown,
                  modules: [__MODULE__])
    end
  end
  
  def behaviour_info(:callbacks), do: [init: 1]
  def behaviour_info(_), do: :undefined

  def init({:spec, spec}), do: init_result(spec)
  def init({module, args}), do: init_result(module.init(args))

  def start_link(module, args), do: :supervisor.start_link(__MODULE__, {module, args})
  def start_link(supname, module, args), do: :supervisor.start_link(supname, __MODULE__, {module, args})
   
  def start_sup(false, supervisor_spec), do: :supervisor.start_link(__MODULE__, {:spec, supervisor_spec})
  def start_sup(reg, supervisor_spec), do: :supervisor.start_link({:local, reg}, __MODULE__, {:spec, supervisor_spec})
                       

  defp init_result(:ignore), do: :ignore
  defp init_result(result), do: {:ok, Strategy.to_supervisor(result)}

end
