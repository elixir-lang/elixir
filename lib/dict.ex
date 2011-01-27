object Dict
  % Construct a new Elixir dictionary by keeping an Erlang dictionary internally.
  def constructor
    { 'dict: Erlang.dict.new }
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
  %       acc << "#{key}: #{value}"
  %     end
  %     list.join(", ") % => "a: 1, b: 1"
  %
  def fold(acc, function)
    Erlang.dict.fold(function, acc, @dict)
  end

  % Returns this dictionary represented as a String.
  %
  % ## Examples
  %
  %     { 'a: 1, 'b: 2 } % => "{'a: 1, 'b: 1}"
  %
  def to_s
    transformer = -> (key, value, acc) ["#{key}: #{value}"|acc]
    "{" + fold([], transformer).join(", ") + "}"
  end
end