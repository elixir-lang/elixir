object OrderedDict
  % Construct a new Elixir dictionary by keeping an Erlang orddict internally.
  def constructor
    { 'orddict: Erlang.orddict.new }
  end

  % Construct a new Dict receiving an Erlang orddict.
  def constructor(orddict)
    { 'orddict: orddict }
  end

  % Calls the given *function* for each key and value of the dictionary with an
  % extra argumen *acc* (short for accumulator). *function* must return a new
  % accumulator passed to the next call. Returns the last accumulator.
  %
  % The function expects three arguments a key, a value and the accumulator.
  % The evaluation order is undefined.
  %
  % ## Examples
  %
  %     dict = { 'a: 1, 'b: 2 }
  %     list = dict.fold [], do (key, value, acc)
  %       ["#{key}: #{value}"|acc]
  %     end
  %     list.join(", ") % => "a: 1, b: 1"
  %
  def fold(acc, function)
    Erlang.orddict.fold(function, acc, @orddict)
  end

  % Calls the given *function* for each key and value. Returns a List
  % with the result of each *function*.
  %
  % The *function* expects a key and value as argument. The evaluation order is undefined.
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
    Erlang.orddict.map(function, @orddict)
  end

  % Returns this dictionary represented as a String.
  %
  % ## Examples
  %
  %     { 'a: 1, 'b: 2 }.inspect % => "{'a: 1, 'b: 1}"
  %
  def inspect
    transformer = -> (key, value, acc) ["#{key.inspect}: #{value.inspect}"|acc]
    "{#{fold([], transformer).join(", ")}}"
  end

  % The same as inspect.
  def to_s
    inspect
  end
end