defmodule Task.Supervised do
  @moduledoc false

  def start_link(mfa) do
    :proc_lib.start_link(__MODULE__, :noreply, [mfa])
  end

  def start_link(mfa, caller, ref) do
    :proc_lib.start_link(__MODULE__, :reply, [mfa, caller, ref])
  end

  def async(mfa, caller, ref) do
    send caller, {ref, apply(mfa)}
  end

  def reply(mfa, caller, ref) do
    :erlang.link(caller)
    :proc_lib.init_ack({:ok, self()})
    send caller, {ref, apply(mfa)}
  end

  def noreply(mfa) do
    :proc_lib.init_ack({:ok, self()})
    apply(mfa)
  end

  defp apply({module, fun, args}) do
    try do
      apply(module, fun, args)
    catch
      :error, reason ->
        exit({reason, System.stacktrace()})
      :throw, value ->
        exit({{:nocatch, value}, System.stacktrace()})
    end
  end
end
