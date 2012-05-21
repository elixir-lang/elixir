defmodule Supervisor.Behavior do
  defmacro __using__(_, _) do
    quote do
          @behavior Supervisor
 
          def start_link, do: Supervisor.start_link({:local, __MODULE__},
                                                    __MODULE__, [])

          defoverridable [start_link: 0]

    end
  end
end