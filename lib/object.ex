object Object
  % TODO Implement to_s and inspect.
  module Methods
    def new(args)
      Erlang.elixir_object_methods.new(self, args)
    end

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

    def constructor
      {:}
    end
  end

  % Object::Methods is automatically mixed and proto'd by Elixir
  % runtime so we can bootstrap the object system.
  % mixin Object::Methods
  % proto Object::Methods
end