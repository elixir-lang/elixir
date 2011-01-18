object Integer
  self.proto Numeric

  module Mixin
  end

  module Proto
  end

  self.mixin Integer::Mixin
  self.proto Integer::Proto
end