object Module
  module Methods
    % Set the following methods to protected.
    Erlang.elixir_module_methods.set_visibility(self, 'protected)

    def __visibility__
      Erlang.elixir_module_methods.get_visibility(self)
    end

    def public
      Erlang.elixir_module_methods.set_visibility(self, 'public)
    end

    def protected
      Erlang.elixir_module_methods.set_visibility(self, 'protected)
    end

    def private
      Erlang.elixir_module_methods.set_visibility(self, 'private)
    end

    def alias_local(old, new, arity)
      Erlang.elixir_module_methods.alias_local(self, __FILE__, old, new, arity)
    end
  end

  mixin Module::Methods
  proto Module::Methods
end