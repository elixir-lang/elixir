object Module
  module Methods
    def public
      Erlang.elixir_object_methods.set_visibility(self, 'public)
    end

    def protected
      Erlang.elixir_object_methods.set_visibility(self, 'protected)
    end

    def private
      Erlang.elixir_object_methods.set_visibility(self, 'private)
    end

    def alias_local(old, new, arity)
      Erlang.elixir_object_methods.alias_local(self, __FILE__, old, new, arity)
    end
  end

  mixin Module::Methods
  proto Module::Methods
end