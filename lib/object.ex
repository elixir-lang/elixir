object Object
  module Methods
    def mixin(module)
      Erlang.elixir_object_methods.mixin(self, module)
    end

    def proto(module)
      Erlang.elixir_object_methods.proto(self, module)
    end

    def mixins
      Erlang.elixir_object_methods.mixins(self)
    end

    def protos
      Erlang.elixir_object_methods.protos(self)
    end

    def ancestors
      Erlang.elixir_object_methods.ancestors(self)
    end

    def dispatch_chain
      Erlang.elixir_object_methods.dispatch_chain(self)
    end
  end

  mixin Object::Methods
  proto Object::Methods
end