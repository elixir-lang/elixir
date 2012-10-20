defmodule GenServer.Behaviour do
  @moduledoc """
  By using this module, you get default GenServer callbacks
  for `init`, `handle_call`, `handle_info`, `handle_cast`,
  `terminate` and `code_change`. Since these functions are
  defined as overridable, they can be customized and fallback
  to the default behaviour by calling `super`.

  This module also tags the behavior as :gen_server. For more
  information on gen_server, please refer to the Erlang
  documentation:

  http://www.erlang.org/doc/man/gen_server.html
  http://www.erlang.org/doc/design_principles/gen_server_concepts.html

  ## Example

      defmodule MyServer do
        use GenServer.Behaviour

        # Callbacks

        def handle_call(:peek, _from, [h|_] = state) do
          { :reply, h, state }
        end

        # Default behaviour
        def handle_call(request, from, config) do
          super(request, from, config)
        end

        def handle_cast({ :push, item }, state) do
          { :noreply, [item|state] }
        end

        # Default cast behaviour
        def handle_cast(request, config) do
          super(request, config)
        end
      end

  """

  @doc false
  defmacro __using__(_) do
    quote location: :keep do
      @behavior :gen_server

      def init(args) do
        { :ok, args }
      end

      def handle_call(_request, _from, state) do
        { :reply, :undef, state }
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