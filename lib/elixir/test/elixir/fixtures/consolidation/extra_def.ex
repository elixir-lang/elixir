defprotocol Protocol.ConsolidationTest.ExtraDef do
  def protocol_fun(term)

  Kernel.def(regular_fun(term), do: term + 1)
end
