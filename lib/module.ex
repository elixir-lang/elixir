object Module
  module Methods
    % Set the following methods to protected.
    Erlang.elixir_module_methods.set_visibility(self, 'protected)

    % Hook invoked whenever this module is added as a mixin.
    % It receives the target object where the mixin is being added
    % as parameter and must return an object of the same kind.
    %
    % ## Example
    %
    % As an example, let's simply create a module that sets an
    % instance variable on the target object:
    %
    %     module Foo
    %       def __added_as_mixin__(base)
    %         base.set_ivar('baz, 13)
    %       end
    %     end
    %
    %     object Baz
    %       mixin Foo
    %       IO.puts @baz   % => 13
    %     end
    %
    def __added_as_mixin__(base)
      base
    end

    % Hook invoked whenever this module is added as a mixin.
    % It receives the target object where the proto is being added
    % as parameter and must return an object of the same kind.
    %
    % Check `__added_as_mixin__` for more examples and information.
    def __added_as_proto__(base)
      base
    end

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

    def callbacks
      if self.__behavior__
        Erlang.elixir_module_methods.set_visibility(self, 'callbacks)
      else
        Erlang.error({'badarg, "cannot define callbacks scope without a behavior specified"})
      end
    end

    def alias_local(old, new, arity)
      Erlang.elixir_module_methods.alias_local(self, __FILE__, old, new, arity)
    end

    % Define an attribute for the Erlang compiled module.
    %
    % This is not meant to be generally used, but only for libraries
    % intending to improve Elixir integration with Erlang.
    def define_module_attribute(key, value)
      Erlang.elixir_module_methods.define_attribute(self, key, value)
    end
  end

  % Module::Methods is automatically mixed and proto'd by Elixir
  % runtime so we can bootstrap the module system.
  % mixin Module::Methods
  % proto Module::Methods
end