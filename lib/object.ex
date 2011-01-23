object Object
  module Methods
    def mixin(module)
      Erlang.elixir_object_methods.mixin(self, module)
    end

    def proto(module)
      Erlang.elixir_object_methods.proto(self, module)
    end

    def __name__
      Erlang.elixir_object_methods.name(self)
    end

    def __parent__
      Erlang.elixir_object_methods.parent(self)
    end

    def __mixins__
      Erlang.elixir_object_methods.mixins(self)
    end

    def __protos__
      Erlang.elixir_object_methods.protos(self)
    end

    def __ancestors__
      Erlang.elixir_object_methods.ancestors(self)
    end

    def __dispatch__
      Erlang.elixir_object_methods.dispatch_chain(self)
    end
  end

  mixin Object::Methods
  proto Object::Methods
end