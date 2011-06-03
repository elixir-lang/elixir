module Tuple
  def empty
    Erlang.elixir_helpers.empty_tuple
  end

  module Instance
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
      Erlang.element(erl_index(number), self)
    end

    % Sets the given element in the tuple. Also accepts negative indexes
    % as in `[]`.
    %
    % ## Examples
    %
    %    {1,2,3}.set(-1, 5) % => {1,2,5}
    %
    def set(number, value)
      Erlang.setelement(erl_index(number), self, value)
    end

    def inspect
      case to_list
      match []
        "Tuple.empty"
      match list
        strings = list.map -> (x) x.inspect.to_char_list
        <<$\{, Erlang.string.join(strings, [$,]).to_bin|binary, $\}>>
      end
    end

    % Returns true if the tuple is empty.
    def empty?
      Erlang.size(self) == 0
    end

    def to_s
      inspect
    end

    def to_list
      Erlang.tuple_to_list(self)
    end

    def to_tuple
      self
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

    private

    def erl_index(number)
      if number < 0
        length + number + 1
      else
        number + 1
      end
    end
  end
end
