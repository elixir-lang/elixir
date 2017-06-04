defmodule Supervisor.Default do
  @moduledoc false

  def init({[{_, _, _, _, _, _} | _] = children, opts}) do
    Supervisor.Spec.supervise(children, opts)
  end

  def init(children, opts) do
    Supervisor.init(children, opts)
  end
end
