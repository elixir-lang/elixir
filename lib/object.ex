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

    def ancestors
      erl.elixir_object_methods.ancestors(self)      
    end

    def dispatch_chain
      erl.elixir_object_methods.dispatch_chain(self)      
    end
  end

  mixin Object::Methods
  proto Object::Methods
end