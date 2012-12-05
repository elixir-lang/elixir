defmodule GenServer.Behaviour do
  @moduledoc """
  This module is a convenience to define GenServer callbacks
  in Elixir. By using this module, you get default implementations
  for the funtions `init/1`, `handle_call/3`, `handle_info/2`,
  `handle_cast/2`, `terminate/2` and `code_change/3`. Since these
  functions are defined as overridable, they can be customized
  and fallback to the default behaviour by calling `super`.

  Starting and sending messages to the gen_server is done
  via Erlang's `:gen_server` module. For more information,
  please refer to the Erlang documentation:

  http://www.erlang.org/doc/man/gen_server.html
  http://www.erlang.org/doc/design_principles/gen_server_concepts.html
  http://learnyousomeerlang.com/clients-and-servers

  ## Example

      defmodule MyServer do
        use GenServer.Behaviour

        # Callbacks

        def handle_call(:pop, _from, [h|t]) do
          { :reply, h, t }
        end

        def handle_call(_request, _from, _config) do
          super
        end

        def handle_cast({ :push, item }, config) do
          { :noreply, [item|config] }
        end

        def handle_cast(_request, _config) do
          super
        end
      end

      { :ok, pid } = :gen_server.start_link(MyServer, [:hello], [])

      :gen_server.call(pid, :pop)
      #=> :hello

      :gen_server.cast(pid, { :push, :world })
      #=> :ok

      :gen_server.call(pid, :pop)
      #=> :world

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behavior :gen_server

      def init(args) do
        { :ok, args }
      end

      def handle_call(_request, _from, state) do
        { :noreply, state }
      end

      def handle_info(_msg, state) do
        { :noreply, state }
      end

      def handle_cast(_msg, state) do
        { :noreply, state }
      end

      def terminate(reason, state) do
        :ok
      end

      def code_change(_old, state, _extra) do
        { :ok, state }
      end

      defoverridable [init: 1, handle_call: 3, handle_info: 2,
        handle_cast: 2, terminate: 2, code_change: 3]
    end
  end
end