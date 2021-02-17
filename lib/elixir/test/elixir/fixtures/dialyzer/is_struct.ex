defmodule Dialyzer.IsStruct do
  def map_literal_atom_literal() do
    is_struct(%Macro.Env{}, Macro.Env)
  end

  def arg_atom_literal(arg) do
    is_struct(arg, Macro.Env)
  end
end
