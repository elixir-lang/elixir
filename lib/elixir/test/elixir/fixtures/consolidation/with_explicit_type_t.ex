defprotocol Protocol.ConsolidationTest.WithExplicitTypeT do
  @type t :: atom()

  def ok(_impl)
end
