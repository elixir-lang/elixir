object Object
  module Methods
    def mixin(module)
      erl.elixir_object_methods.mixin(self, module)
    end

    def proto(module)
      erl.elixir_object_methods.proto(self, module)
    end

    def mixins
      erl.elixir_object_methods.mixins(self)
    end

    def protos
      erl.elixir_object_methods.protos(self)
    end
  end

  self.mixin Object::Methods
  self.proto Object::Methods
end