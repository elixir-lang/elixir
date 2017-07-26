defmodule Supervisor.Default do
  @moduledoc false

  def init({children, opts}) do
    Supervisor.init(children, opts)
  end
end
