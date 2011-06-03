module Method
  module Instance
    attr_reader ['binding, 'owner, 'name, 'arity]

    % Dynamically define call and [] as methods that receives
    % up to 20 arguments and forward them to the function.
    20.times [], do (i, acc)
      module_eval __FILE__, __LINE__, ~~ELIXIR
def [](#{acc.join(",")})
  Erlang.apply(@owner, @name, [#{["@binding"|acc].join(",")}])
end

def call(#{acc.join(",")})
  Erlang.apply(@owner, @name, [#{["@binding"|acc].join(",")}])
end
~~

      ["v#{i}"|acc]
    end

    def __bound__(binding, owner, name, arity)
      @('binding: binding, 'owner: owner, 'name: name, 'arity: arity)
    end

    def bind(object)
      @('binding, object)
    end

    def apply(args)
      Erlang.apply(@owner, @name, [@binding|args])
    end
  end
end