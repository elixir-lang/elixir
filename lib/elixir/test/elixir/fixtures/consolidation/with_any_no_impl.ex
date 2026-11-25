defprotocol Protocol.ConsolidationTest.WithAnyNoImpl do
  @fallback_to_any true
  def ok(term)
end
