object String
  % Initializes a string by keeping its internal list representation.
  def constructor(list)
    { 'list: list }
  end

  % Concatenate two strings.
  %
  % ## Examples
  %
  %     "eli" + "xir" % => "elixir"
  %
  def +(another)
    String.new(@list + another.to_list)
  end

  % Returns the length of the string. All strings parsed by the
  % interpreter are handled as utf-8. Any I/O driver should be
  % responsible to convert to utf-8.
  %
  % ## Examples
  %
  %     "elixir".length % => 6
  %     "josé".length   % => 4
  %
  def length
    Erlang.length(@list)
  end

  % Returns the list representation of this String.
  def to_list
    @list
  end

  % Returns a string representation of this string.
  %
  % ## Examples
  %
  %     "elixir".inspect % => "\"elixir\""
  %
  def inspect
    String.new [$"|@list] + [$"]
  end

  % Returns the string itself.
  def to_s
    self
  end
end
