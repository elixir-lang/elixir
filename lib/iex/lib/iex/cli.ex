defmodule IEx.CLI do
  @moduledoc false

  @doc """
  In order to work properly, IEx needs to be set as the
  proper `-user` when starting the Erlang VM and we do so
  by pointing exactly to this function.

  If possible, Elixir will start a tty (smart terminal)
  which makes all control commands available in tty
  available to the developer.

  In case `tty` is not available (for example, Windows),
  a dumb terminal version is started instead.
  """
  def start do
    if tty_works? do
      tty
    else
      :user.start
      IO.puts "Warning: could not run smart terminal, falling back to dumb one"
      config = [dot_iex_path: find_dot_iex(:init.get_plain_arguments)]
      IEx.start(config, fn -> :elixir.start_cli end)
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
    plain_args = :init.get_plain_arguments

    config = [dot_iex_path: find_dot_iex(plain_args)]
    function = fn ->
      IEx.start(config, fn -> :elixir.start_cli end)
    end

    args =
      if remote = get_remsh(plain_args) do
        unless is_alive do
          function = fn ->
            IO.puts(:stderr, "In order to use --remsh, you need to name the current node using --name or --sname. Aborting...")
            System.halt(1)
          end

          { :erlang, :apply, [function, []] }
        else
          { remote, :erlang, :apply, [function, []] }
        end
      else
        { :erlang, :apply, [function, []] }
      end

    :user_drv.start([:"tty_sl -c -e", args])
  end

  defp find_dot_iex(['--dot-iex',h|_]), do: :unicode.characters_to_binary(h)
  defp find_dot_iex([_|t]), do: find_dot_iex(t)
  defp find_dot_iex([]), do: nil

  defp get_remsh(['--remsh',h|_]), do: list_to_atom(h)
  defp get_remsh([_|t]), do: get_remsh(t)
  defp get_remsh([]), do: nil
end
