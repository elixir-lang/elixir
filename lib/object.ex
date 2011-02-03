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

    % We could mark the methods below as protected, but, as everything
    % inherits from Object, the visibility would never take effect and
    % would just affect performance instead.
    % protected

    def constructor
      {:}
    end

    def get_ivar(name)
      Erlang.elixir_object_methods.get_ivar(self, name)
    end

    def set_ivar(name, value)
      Erlang.elixir_object_methods.set_ivar(self, name, value)
    end

    def public
      Erlang.elixir_object_methods.set_visibility(self, 'public)
    end

    def protected
      Erlang.elixir_object_methods.set_visibility(self, 'protected)
    end

    def private
      Erlang.elixir_object_methods.set_visibility(self, 'private)
    end

    def alias_local_method(old, new, arity)
      Erlang.elixir_object_methods.alias_local_method(self, old, new, arity)
    end
  end

  % Object::Methods is automatically mixed and proto'd by Elixir
  % runtime so we can bootstrap the object system.
  % mixin Object::Methods
  % proto Object::Methods
end