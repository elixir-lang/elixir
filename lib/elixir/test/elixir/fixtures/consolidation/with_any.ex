defprotocol Protocol.ConsolidationTest.WithAny do
  @fallback_to_any true
  @doc "Ok"
  def ok(term)
end
