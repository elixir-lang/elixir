module OrderedDict
  % Generates a new OrderedDict from a list of tuples
  %
  % ## Examples
  %
  %     { 'a: 1, 'b: 2 } = OrderedDict.from_list(['a/1, 'b/2])
  %
  def from_list(list)
    { 'elixir_orddict__, Erlang.orddict.from_list(list) }
  end

  % Return a new Elixir OrderedDict. Remember that new is
  % special cased by the compiler to receive an array as argument.
  def new([])
    { 'elixir_orddict__, Erlang.orddict.new() }
  end

  % Return a new Elixir OrderedDict given the dictionary.
  def new([orddict])
    { 'elixir_orddict__, orddict }
  end

  module Instance
    % Updates the given *key* in list according to te given *function*.
    % If no key exist, raises an error. You can use update/3 if you want
    % to set an initial value if none exists.
    %
    % ## Examples
    %
    %     dict = { 'vowels: ['a, 'e, 'i, 'o] }
    %     new_dict = dict.update 'vowels, _.push('u)
    %     new_dict['vowels] % => ['a, 'e, 'i, 'o, 'u]
    %
    def update(key, function)
      OrderedDict.new Erlang.orddict.update(key, function, orddict)
    end

    % Merge one dict into the other.
    %
    % ## Examples
    %
    %     { 'a: 3, 'b: 2 } = { 'a: 1 }.merge { 'b: 2, 'a: 3 }, -> (_, v1, _) v1
    %
    def merge(other)
      function = -> (_k, _v1, v2) v2
      OrderedDict.new Erlang.orddict.merge(function, orddict, other.to_list)
    end

    % Merge one dict into the other according to the given function. The function
    % is invoked when both dicts have the same keys with the key and both values
    % as arguments and should return the result given by the conflict of such keys.
    %
    % ## Examples
    %
    % The example provides a reverse merge, where the first dict is merged into
    % the one given as argument:
    %
    %     { 'a: 1, 'b: 2 } = { 'a: 1 }.merge { 'b: 2, 'a: 3 }, -> (_k, v1, _v2) v1
    %
    def merge(other, function)
      OrderedDict.new Erlang.orddict.merge(function, orddict, other.to_list)
    end

    % The same as update/2, but if no value exists, *initial* is used.
    %
    % ## Examples
    %
    %     dict = {}.update('values, [], _.push(1))
    %     dict['values]  % => []
    %
    %     dict.update('values, [], _.push(1))
    %     dict['values]  % => [1]
    %
    def update(key, initial, function)
      OrderedDict.new Erlang.orddict.update(key, function, initial, orddict)
    end

    % Retrieves the given key from the OrderedDict. Returns [] if key does not exist.
    %
    % ## Examples
    %
    %     { 1: 2, 3: 4}[1]  % => 2
    %     { 1: 2, 3: 4}[5]  % => []
    %
    def [](key)
      case Erlang.orddict.find(key, orddict)
      match {'ok, value}
        value
      match 'error
      end
    end
    alias_local '[], 'get, 1

    % Returns a boolean if the ordered dict has the given key or not.
    %
    % ## Examples
    %
    %     { 'a: 1 }.key?('a) % => true
    %
    def key?(key)
      case Erlang.orddict.find(key, orddict)
      match {'ok, value}
        true
      match 'error
        false
      end
    end

    % Stores the given *value* in *key* in the dictionary.
    %
    % ## Examples
    %
    %     {}.set('a, 'b)  % => { 'a: 'b }
    %
    def set(key, value)
      OrderedDict.new Erlang.orddict.store(key, value, orddict)
    end
    alias_local 'set, 'store, 2

    % Stores the given *value* in *key* in the dictionary if none is set yet.
    %
    % ## Examples
    %
    %     {'a: b}.set_new('a,'c)  % => { 'a: 'b }
    %     {'a: b}.set_new('b,'c)  % => { 'a: 'b, 'b: c }
    %
    def set_new(key, value)
      if key?(key)
        self
      else
        set(key, value)
      end
    end
    alias_local 'set_new, 'store_new, 2

    % Calls the given *function* for each key and value of the dictionary with an
    % extra argumen *acc* (short for accumulator). *function* must return a new
    % accumulator passed to the next call. Returns the last accumulator.
    %
    % The function expects three arguments a key, a value and the accumulator.
    %
    % ## Examples
    %
    %     dict = { 'a: 1, 'b: 2 }
    %     list = dict.fold [], do (key, value, acc)
    %       ["#{key}: #{value}"|acc]
    %     end
    %     list.join(", ") % => "b: 2, a: 1"
    %
    def fold(acc, function)
      Erlang.orddict.fold(function, acc, orddict)
    end

    % Deletes the given key from the Ordered Dict returning the new dict.
    %
    % ## Examples
    %
    %     { 'a: 1, 'b: 2 }.delete 'a  % => { 'b: 2 }
    %
    def delete(key)
      OrderedDict.new Erlang.orddict.erase(key, orddict)
    end

    % Calls the given *function* for each key and value. Returns a List
    % with the result of each *function*.
    %
    % The *function* expects a key and value as argument.
    %
    % ## Examples
    %
    %     dict = { 'a: 1, 'b: 2 }
    %     new_dict = dict.map do (key, value)
    %       value * 2
    %     end
    %     new_dict % => { 'a: 2, 'b: 4 }
    %
    def map(function)
      OrderedDict.new Erlang.orddict.map(function, orddict)
    end

    % Loops for each key-value pair in the dictionary.
    %
    % ## Examples
    %
    %     dict = { 'a: 1, 'b: 2 }
    %     dict.each do (key, value)
    %       IO.puts "#{key}: #{value}"
    %     end
    %
    def each(function)
      to_list.each -> ({x, y}) function.(x, y)
      self
    end

    % Returns this dictionary represented as a String.
    %
    % ## Examples
    %
    %     { 'a: 1, 'b: 2 }.inspect % => "{'a: 1, 'b: 1}"
    %
    def inspect
      inspect(orddict)
    end

    % The same as inspect.
    def to_s
      inspect
    end

    % Returns true if the ordered dict is empty.
    def empty?
      orddict == []
    end

    % Converts this OrderedDict to a list. The return list is ordered.
    %
    % == Examples
    %
    %     [{'a, 1},{'b, 2}] = { 'a: 1, 'b: 2 }.to_list
    %
    def to_list
      orddict
    end

    private

    def inspect([])
      "{}"
    end

    def inspect(_)
      transformer = -> (key, value, acc) ["#{key.inspect}: #{value.inspect}"|acc]
      "{#{fold([], transformer).reverse.join(", ")}}"
    end

    def orddict
      Erlang.element(2, self)
    end
  end
end