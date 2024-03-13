defprotocol Protocol.ConsolidationTest.WithDeclaredTypes do
  @type tuple_type :: tuple()
  @type atom_type :: atom()
  @type list_type :: list()
  @type bitstring_type :: bitstring()
  @type integer_type :: integer()
  @type float_type :: float()
  @type function_type :: function()
  @type pid_type :: pid()
  @type map_type :: map()
  @type port_type :: port()
  @type reference_type :: reference()
  @type implstruct_type :: %{__struct__: Protocol.ConsolidationTest.ImplStruct}

  @type impl_types ::
          tuple()
          | atom()
          | list()
          | bitstring()
          | integer()
          | float()
          | function()
          | pid()
          | map()
          | port()
          | reference()
          | %{__struct__: Protocol.ConsolidationTest.ImplStruct}

  def ok(impl)
end
