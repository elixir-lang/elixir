defmodule Env.Compiler do
  @moduledoc false

  defmacro generate do
    quote do
      @moduledoc """
      Provides functions for obtaining information about Elixir runtime.
      """

      @doc """
      Returns a tuple { Elixir version, commit sha-1, build date }
      """
      def version do
        { Code.version,
          unquote(get_head_sha),
          unquote(get_date) }
      end

      @doc """
      Returns the list of command-line arguments passed to the program.
      """
      def argv do
        Erlang.gen_server.call(:elixir_code_server, :argv)
      end
    end
  end

  defp get_head_sha do
    trim :os.cmd 'git rev-parse HEAD'
  end

  defp get_date do
    trim :os.cmd 'TZ=GMT date'
  end

  defp trim(string) do
    :re.replace string, '\n' , '', [{:return,:list}]
  end
end

defmodule Env do
  require Env.Compiler
  Env.Compiler.generate
end
