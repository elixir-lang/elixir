IO.puts("======")

defmodule BigStruct do
  defstruct [
    :f1,
    :f2,
    :f3,
    :f4,
    :f5,
    :f6,
    :f7,
    :f8,
    :f9,
    :f10,
    :f11,
    :f12,
    :f13,
    :f14,
    :f15,
    :f16,
    :f17,
    :f18,
    :f19,
    :f20,
    :f21,
    :f22,
    :f23,
    :f24,
    :f25,
    :f26,
    :f27,
    :f28,
    :f29,
    :f30,
    :f31,
    :f32,
    :f33,
    :value,
    :schema
  ]

  def cast(%__MODULE__{value: nil, schema: %{k1: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k2: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k3: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k4: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k5: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k6: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k7: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k8: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k9: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k10: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k11: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k12: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k13: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k14: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k15: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k16: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k17: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k18: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k19: _}}), do: :ok
  def cast(%__MODULE__{value: nil, schema: %{k20: _}}), do: :ok
  # This different clause avoids optimizations from kick in many cases
  def cast(%__MODULE__{schema: %{target_key: x}}), do: x
end
