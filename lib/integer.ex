module Integer
  module Instance
    mixin Numeric

    def inspect
      to_s
    end

    % Iterates the given function n times, passing values from zero
    % to n - 1.
    %
    % ## Examples
    %
    %    5.times -> (x) IO.puts x % => 0 1 2 3 4
    %
    def times(function)
      if function.arity == 0
        times_0(self, 0, function)
      else
        times_1(self, 0, function)
      end
      self
    end

    % Iterates the given function n times, passing values from zero
    % to n - 1. Also has an accumulator similar to fold to store a
    % value between computations.
    %
    % ## Examples
    %
    %    5.times(0, -> (acc, x) acc + x) % => 10
    %
    def times(acc, function)
      times_2(self, 0, function, acc)
    end

    ['div, 'rem].each do (op)
      define_erlang_method __FILE__, __LINE__, op, 1, [
        {
          'clause, __LINE__, [{'var, __LINE__, 'other}], [], [
            { 'op, __LINE__, op, {'var, __LINE__, 'self}, {'var, __LINE__, 'other} }
          ]
        }
      ]
    end

    % Convert an integer to the equivalent bit string.
    %
    % ## Examples
    %
    %    65.chr % => "A"
    %    97.chr % => "a"
    %
    def chr
      <<self>>
    end

    % Conver an integer to a char list of its digits.
    %
    % ## Examples
    %
    %    123.to_char_list % => [49, 50, 51]
    %
    def to_char_list
      Erlang.integer_to_list(self)
    end

    def to_s
      Erlang.integer_to_list(self).to_bin
    end

    private

    def times_0(limit, limit, _function)
    end

    def times_0(limit, counter, function)
      function.()
      times_0(limit, 1 + counter, function)
    end

    def times_1(limit, limit, _function)
    end

    def times_1(limit, counter, function)
      function.(counter)
      times_1(limit, 1 + counter, function)
    end

    def times_2(limit, limit, _function, acc)
      acc
    end

    def times_2(limit, counter, function, acc)
      new_acc = function.(counter, acc)
      times_2(limit, 1 + counter, function, new_acc)
    end
  end
end