% elixir: cache

object OrderedDict
  % Implement OrderedDict as a record. This is done mainly
  % to have improved performance for the equality operator.
  module Mixin
    % Return a new Elixir OrderedDict. Remember that new is
    % special cased by the compiler to receive an array as argument.
    def new([])
      { 'elixir_orddict__, Erlang.orddict.new() }
    end

    % Return a new Elixir OrderedDict given the dictionary.
    def new([orddict])
      { 'elixir_orddict__, orddict }
    end
  end

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

  % The same as update/2, but if no value exists, *initial* is used.
  %
  % ## Examples
  %
  %     dict = {:}.update('values, [], _.push(1))
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

  % Stores the given *key* and *value* in the dictionary.
  %
  % ## Examples
  %
  %     {:}.set('a, 'b)  % => { 'a: 'b }
  %
  def set(key, value)
    OrderedDict.new Erlang.orddict.store(key, value, orddict)
  end
  alias_local 'set, 'store, 2

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
  %     new_dict = dict.map do (key, value, acc)
  %       value * 2
  %     end
  %     new_dict % => { 'a: 2, 'b: 4 }
  %
  def map(function)
    OrderedDict.new Erlang.orddict.map(function, orddict)
  end

  % Returns this dictionary represented as a String.
  %
  % ## Examples
  %
  %     { 'a: 1, 'b: 2 }.inspect % => "{'a: 1, 'b: 1}"
  %
  def inspect
    transformer = -> (key, value, acc) ["#{key.inspect}: #{value.inspect}"|acc]
    "{#{fold([], transformer).reverse.join(", ")}}"
  end

  % The same as inspect.
  def to_s
    inspect
  end

  private

  def orddict
    Erlang.element(2, self)
  end
end
