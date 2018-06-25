defmodule ExUnit.Pattern do
  defstruct [:val, :vars, :pins, :meta]


  @type t :: %__MODULE__{
          val: any(),
          meta: :list | :%{} | :{} | :|,
          vars: [key: atom()],
          pins: [key: atom()],
        }

  def new(lh_pattern, pins, unbound_vars) when is_list(pins) and is_list(unbound_vars) do
    %__MODULE__{
      val: lh_pattern,
      meta: get_meta(lh_pattern),
      pins: pins,
      vars: unbound_vars,
    }
  end

  def new(lh_pattern, %__MODULE__{} = parent, meta)  do
    %__MODULE__{
      val: lh_pattern,
      meta: meta,
      pins: parent.pins,
      vars: parent.vars,
    }
  end

  def new(lh_pattern, %__MODULE__{} = parent)  do
    %__MODULE__{
      val: lh_pattern,
      meta: get_meta(lh_pattern),
      pins: parent.pins,
      vars: parent.vars,
    }
  end


  defp get_meta(node) when is_list(node), do: :list
  defp get_meta(_), do: nil
end
