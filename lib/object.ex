object Object
  module Methods
    % Create a new object using the current object as parent
    %
    % ## Example
    %
    %     obj = Object.new
    %     obj.__parent__ %=> 'Object
    %
    % ## Notes
    %
    % The new method is special cased by the compiler to receive
    % all arguments wrapped into a single array.
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

    def __ivars__
      Dict.new Erlang.elixir_object_methods.data(self)
    end

    def __ancestors__
      Erlang.elixir_object_methods.ancestors(self)
    end

    def inspect
      name = __name__
      if name
        name.to_s
      else
        "<#{__parent__} #{__ivars__.inspect}>"
      end
    end

    def to_s
      self.inspect
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

    def __public_proto_methods__
      Erlang.elixir_object_methods.public_proto_methods(self)
    end

    def send(method)
      send(method, [])
    end

    def send(method, args)
      Erlang.elixir_dispatch.dispatch(false, self, method, args)
    end

    def catch(function)
      filter_stacktrace Erlang.elixir_object_methods.function_catch(function)
    end

    % Set the following methods to private.
    Erlang.elixir_object_methods.set_visibility(self, 'private)

    def filter_stacktrace({ 'EXIT, { reason, stacktrace } })
      regexp = ~r(^[A-Z])
      newtrace = stacktrace.foldr [], do ({module, function, arity}, acc)
        if regexp.match?(module)
          [{module, function, arity - 1}|acc]
        else
          acc
        end
      end
      { 'EXIT, { reason, newtrace } }
    end

    def filter_stacktrace(other)
      other
    end
  end

  % Object::Methods is automatically mixed and proto'd by Elixir
  % runtime so we can bootstrap the object system.
  % mixin Object::Methods
  % proto Object::Methods
end