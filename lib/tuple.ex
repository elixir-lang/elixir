% elixir: cache

object Tuple
  % Retrieves an item from the tuple. Negative indexes are allowed
  % and they retrieve the element in the reverse order. Out of bound
  % indexes raises 'function_clause error.
  %
  % ## Examples
  %
  %     {1,2,3}[0] % => 1
  %     {1,2,3}[1] % => 2
  %     {1,2,3}[2] % => 3
  %     {1,2,3}[3] % => Raises 'badarg error
  %   
  %     {1,2,3}[-1] % => 3
  %     {1,2,3}[-2] % => 2
  %     {1,2,3}[-3] % => 1
  %     {1,2,3}[-4] % => Raises 'badarg error
  %
  def [](number)
    if number < 0
      Erlang.element(length + number + 1, self)
    else
      Erlang.element(number + 1, self)
    end
  end

  def inspect
    strings = to_list.map -> (x) x.inspect.to_char_list
    "{#{String.new Erlang.string.join(strings, [$,, $\s])}}"
  end

  def to_s
    inspect
  end

  def to_list
    Erlang.tuple_to_list(self)
  end

  % Returns the tuple length. Also aliased to size.
  %
  % ## Examples
  %
  %     {1,2,3}.length % => 3
  %     {}.size        % => 0
  %
  def length
    Erlang.size(self)
  end
  alias_local 'length, 'size, 0
end
