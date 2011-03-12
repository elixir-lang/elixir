% elixir: cache

object Module
  module Methods
    % Returns the behavior for the self.
    def __behavior__
      Erlang.elixir_callbacks.behavior(self)
    end

    % Set the following methods to protected.
    Erlang.elixir_module_methods.set_visibility(self, 'protected)

    % Delegate the given methods to the given expression.
    %
    % ## Examples
    %
    %     module Counter
    %       def one; 1; end
    %       def two; 2; end
    %       def three; 3; end
    %       def sum(a, b) a+b; end
    %     end
    %
    %     module Delegator
    %       delegate ['one/0, 'two/0, 'three/0, 'sum/2], 'to: "Counter"
    %     end
    %
    %     Delegator.one       % => 1
    %     Delegator.sum(1, 2) % => 3
    %
    % Notice that the value given to 'to can be any expression:
    %
    %     module Three
    %       delegate ['abs/0], 'to: "(2-5)"
    %     end
    %
    %     Three.abs  % => 3
    %
    def delegate(pairs, options)
      object = options['to]

      pairs.each do ({name, arity})
        args = arity.times [], do (i, acc)
          ["x#{i}"|acc]
        end

        args_string = args.join(",")

        module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
  def #{name}(#{args_string})
    #{object}.#{name}(#{args_string})
  end
~~
      end
    end

    % Receives a list of names and define a method for each name that
    % reads its respective instance variable.
    %
    % ## Example
    %
    %     object Car
    %       attr_reader ['color]
    %
    %       def constructor(color)
    %         { 'color: color }
    %       end
    %     end
    %
    %     car = Car.new 'red
    %     car.color   % => 'red
    %
    def attr_reader(names)
      names.each do (name)
        module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
  def #{name}
    @#{name}
  end
~~
      end
    end

    def attr_writer(names)
      names.each do (name)
        module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
  def #{name}(value)
    self.set_ivar '#{name}, value
  end
~~
      end
    end

    def attr_accessor(names)
      attr_reader names
      attr_writer names
    end

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

    % Returns the current method visibility.
    def __visibility__
      Erlang.elixir_module_methods.get_visibility(self)
    end

    % Mark all methods defined next as public.
    def public
      Erlang.elixir_module_methods.set_visibility(self, 'public)
    end

    % Mark all methods defined next as protected.
    def protected
      Erlang.elixir_module_methods.set_visibility(self, 'protected)
    end

    % Mark all methods defined next as private.
    def private
      Erlang.elixir_module_methods.set_visibility(self, 'private)
    end

    % Receives a file, line and evaluates the given string in the context
    % of the module. This is good for dynamic method definition:
    %
    % ## Examples
    %
    %     module MyMethods
    %
    %       ["foo", "bar", "baz"].each -> (m)
    %         self.module_eval __FILE__, __LINE__ + 1, ~~ELIXIR
    %       def #{m}
    %         @#{m}
    %       end
    %     ~~
    %       end
    %
    %     end
    % 
    def module_eval(file, line, string)
      Erlang.elixir_module_methods.module_eval(self, string.to_char_list, file.to_char_list, line)
    end

    % Alias a local method. Aliasing a method defined in another module is done
    % by delegation.
    def alias_local(old, new, arity)
      Erlang.elixir_module_methods.alias_local(self, __FILE__, old, new, arity)
    end
  end

  % Module::Methods is automatically mixed and proto'd by Elixir
  % runtime so we can bootstrap the module system.
  % mixin Module::Methods
  % proto Module::Methods
end