object Float
  module Mixin
  end

  module Proto
  end

  self.mixin Float::Mixin
  self.proto Float::Proto
  self.proto Numeric
end