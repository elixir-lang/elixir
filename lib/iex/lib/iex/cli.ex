defmodule IEx.CLI do
  @moduledoc false

  @doc """
  Starts IEx checking if tty is available or not.
  If so, invoke tty, otherwise go with the simple iex.
  """
  def start do
    if tty_works? do
      tty
    else
      :user.start
      IO.puts "Warning: could not run smart terminal, falling back to dumb one"
      IEx.start([], fn -> :elixir.start_cli end)
    end
  end

  # Check if tty works. If it does not, we fall back to the
  # simple/dumb terminal. This is starting the linked in
  # driver twice, it would be nice and appropriate if we had
  # to do it just once.
  defp tty_works? do
    try do
      port = Port.open { :spawn, :"tty_sl -c -e" }, [:eof]
      Port.close(port)
    catch
      _, _ -> false
    end
  end

  defp tty do
    function = fn ->
      IEx.start([], fn -> :elixir.start_cli end)
    end

    args =
      if remote = get_remsh(:init.get_plain_arguments) do
        unless is_alive do
          raise ArgumentError, message: "In order to use --remsh, you need to name the current node"
        end
        { remote, :erlang, :apply, [function, []] }
      else
        { :erlang, :apply, [function, []] }
      end

    :user_drv.start([:"tty_sl -c -e", args])
  end

  defp get_remsh(['--remsh',h|_]), do: list_to_atom(h)
  defp get_remsh([_|t]), do: get_remsh(t)
  defp get_remsh([]), do: nil
end
