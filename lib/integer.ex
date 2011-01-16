object Integer
  module Mixin
  end

  module Proto
  end

  self.mixin Integer::Mixin
  self.proto Integer::Proto
  self.proto Numeric
end