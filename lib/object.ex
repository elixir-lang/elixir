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

    def __parent_name__
      Erlang.elixir_object_methods.parent_name(self)
    end

    def __parent__
      Erlang.elixir_object_methods.parent(self)
    end

    % Returns a list of atoms representing all mixins for the current object.
    % The mixins chain is retrieved by getting the current object mixins and
    % appending all protos from its parents.
    %
    % ## Example
    %
    % Imagine the following definition:
    %
    %     object Foo
    %       mixin SomeMethods
    %       proto MoreMethods
    %     end
    %
    % The mixins chain for Foo is:
    %
    %     Foo.__mixins__  % => ['SomeMethods, 'Object::Methods]
    %
    % Where `SomeMethods` is a module mixed into `Foo` and the `Object::Methods`
    % is a module added as proto on `Foo` parent, which is the object `Object`:
    %
    %     Foo.__parent__  % => 'Object
    %
    % The mixins for `Foo.new` are these:
    %
    %     Foo.new.__mixins__ %=> ['MoreMethods, 'Object::Methods]
    %
    % Where `MoreMethods` is a module added as proto to `Foo.new` parent (which is `Foo`)
    % and `Object::Methods` is a proto on `Foo.new` grandparent (which is `Object`).
    %
    % If we added a mixin to `Foo.new`, we would have the following result:
    %
    %     Foo.new.mixin(MuchMoreMethods).__mixins__ %=> ['MuchMoreMethods, 'MoreMethods, 'Object::Methods]
    %
    % In other words, calculating the mixins chain is as simple as:
    %
    %     [self.exclusive_mixins, self.parent.exclusive_protos, self.parent.parent.exclusive_protos, ...]
    %
    % Until parent becomes empty (`Object` is the only object that does not have a parent).
    %
    % Notice that we don't have methods called `exclusive_mixins` and `exclusive_protos`,
    % they are mentioned just as examples.
    %
    % == Mixins inside object definitions
    %
    % In Elixir, all methods are carried in modules. Regular objects does not have methods per-se.
    % However, for convenience, Elixir allows you to define methods inside the object definition:
    %
    %     object Foo
    %       def bar
    %         'baz
    %       end
    %     end
    %
    % What happens internally is that Elixir automatically creates a module named `Foo::Proto`
    % and automatically adds it as `Foo` proto. In order for this to work properly, Elixir
    % adds `Module::Methods` as mixin during the object definition, but it is removed from
    % the mixins chain after the object is defined:
    %
    %     object Foo
    %       __mixins__   % => ['Module::Methods, 'Object::Methods]
    %     end
    %
    %     Foo.__mixins__ % => ['Object::Methods]
    %
    def __mixins__
      Erlang.elixir_object_methods.mixins(self)
    end

    % Returns a list of atoms representing all proto for the current object.
    % The mixins chain is retrieved by getting the current object protos and
    % appending all protos from its parents.
    %
    % ## Example
    %
    % Imagine the following definition:
    %
    %     object Foo
    %       mixin SomeMethods
    %       proto MoreMethods
    %     end
    %
    % The protos chain for Foo is:
    %
    %     Foo.__protos__  % => ['MoreMethods, 'Object::Methods]
    %     Foo.new.__protos__  % => ['MoreMethods, 'Object::Methods]
    %
    % The lookup for protos happens in a similar fashion to mixins:
    %
    %     [self.exclusive_protos, self.parent.exclusive_protos, self.parent.parent.exclusive_protos, ...]
    %
    % Read the documentation for `__mixins__` for more information.
    def __protos__
      Erlang.elixir_object_methods.protos(self)
    end

    def __ancestors__
      Erlang.elixir_object_methods.ancestors(self)
    end

    def inspect
      name = __name__
      if name != []
        name.to_s
      else
        "<#{__parent_name__} #{get_ivars.inspect}>"
      end
    end

    def to_s
      self.inspect
    end

    def initialize
      self
    end

    def get_ivar(name)
      Erlang.elixir_object_methods.get_ivar(self, name)
    end

    % Returns a `Dict` with all variable names and values as its key-values.
    %
    % ## Example
    %
    %     object Foo
    %       def initialize
    %         @('bar: 1, 'baz: 2)
    %       end
    %     end
    %
    %     Foo.new.__ivars__ % => { 'bar: 1, 'baz: 2 }
    %
    def get_ivars
      OrderedDict.new Erlang.elixir_object_methods.data(self)
    end

    def set_ivar(name, value)
      Erlang.elixir_object_methods.set_ivar(self, name, value)
    end

    def set_ivars(value)
      Erlang.elixir_object_methods.set_ivars(self, value)
    end

    def update_ivar(name, fun)
      Erlang.elixir_object_methods.update_ivar(self, name, fun)
    end

    def update_ivar(name, initial, fun)
      Erlang.elixir_object_methods.update_ivar(self, name, initial, fun)
    end

    def __send__(method, args := [])
      Erlang.elixir_dispatch.dispatch(self, method, args)
    end

    % Those methods are related to methods introspection.

    def __proto_methods__
      Erlang.elixir_methods.proto_methods(self)
    end

    def __mixin_methods__
      Erlang.elixir_methods.mixin_methods(self)
    end

    % Exceptions related methods

    def __stacktrace__
      filter_stacktrace Erlang.get_stacktrace
    end

    def error(reason)
      Erlang.error(reason)
    end

    def throw(reason)
      Erlang.throw(reason)
    end

    def exit(reason)
      Erlang.exit(reason)
    end

    def catch!(function)
      filter_catch_stacktrace Erlang.elixir_object_methods.function_catch(function)
    end

    def method_missing('initialize, args)
      IO.puts "[ELIXIR] Defining 'constructor' is deprecated. Please define 'initialize' and return an object."
      @(__send__('constructor, args))
    end

    def method_missing(method, args)
      error { 'nomethod, {self, method, Erlang.length(args)} }
    end

    % Set the following methods to private.
    Erlang.elixir_module_methods.set_visibility(self, 'private)

    def filter_stacktrace(stacktrace)
      filter_stacktrace(stacktrace, [])
    end

    def filter_stacktrace([{raw_module, function, raw_arity}|t], buffer)
      if filtered = filter_stacktrace_module(raw_module.to_char_list)
        module = filtered
        arity = if raw_arity.__parent_name__ == 'Integer
          raw_arity - 1
        else
          raw_arity
        end
      else
        module = raw_module
        arity = raw_arity
      end

      filter_stacktrace t, [{module, function, arity}|buffer]
    end

    def filter_stacktrace([], buffer)
      buffer.reverse
    end

    def filter_stacktrace_module([$e, $x, h|t]) when h >= $A andalso h <= $Z
      Atom.from_char_list [h|t]
    end

    def filter_stacktrace_module(_)
      nil
    end

    def filter_catch_stacktrace({ 'EXIT, { reason, stacktrace } })
      { 'EXIT, { reason, filter_stacktrace(stacktrace) } }
    end

    def filter_catch_stacktrace(other)
      other
    end
  end

  % Object::Methods is automatically mixed and proto'd by Elixir
  % runtime so we can bootstrap the object system.
  % mixin Object::Methods
  % proto Object::Methods
end