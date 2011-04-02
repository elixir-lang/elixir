% elixir: cache [fixtures/dependency_2]

module Dependency1
  def value
    Dependency2.value
  end
end