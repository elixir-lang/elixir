defprotocol Protocol.ConsolidationTest.Sample do
  @type t :: any
  @doc "Ok"
  @deprecated "Reason"
  @spec ok(t) :: boolean
  def ok(term)

  # Not a protocol function. While this is not "officially" supported,
  # it does happen in practice, so we need to make sure we preserve
  # its signature.
  Kernel.def(regular_fun(term), do: term + 1)
end
