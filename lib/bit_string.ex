module BitString
  module Instance
    def to_list
      Erlang.binary_to_list(self)
    end

    def inspect
      Erlang.io_lib.format($"~w", [self]).to_bin
    end

    def to_s
      inspect
    end

    % Returns the binary length. Also aliased to size.
    %
    % ## Examples
    %
    %     <<1,2,3>>.length % => 3
    %     <<>>.size        % => 0
    %
    def length
      Erlang.size(self)
    end
    alias_local 'length, 'size, 0
  end
end