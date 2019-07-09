defprotocol Protocol.ConsolidationTest.Sample do
  @type t :: any
  @doc "Ok"
  @deprecated "Reason"
  @spec ok(t) :: boolean
  def ok(term)
end
