defmodule Application.Behaviour do
  @moduledoc false

  defmacro __using__(_) do
    # IO.write :stderr, "use Application.Behaviour is deprecated, please use Application instead\n#{Exception.format_stacktrace}"
    quote location: :keep do
      @behaviour :application

      @doc false
      def stop(_state) do
        :ok
      end

      defoverridable [stop: 1]
    end
  end
end
