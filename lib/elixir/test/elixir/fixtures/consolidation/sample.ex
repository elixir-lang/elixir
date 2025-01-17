defprotocol Protocol.ConsolidationTest.Sample do
  @type t :: any
  @doc "Ok"
  @deprecated "Reason"
  @spec ok(t) :: boolean
  def ok(term)

  # Not a protocol function
  Kernel.def(regular_fun(term), do: term + 1)
end
