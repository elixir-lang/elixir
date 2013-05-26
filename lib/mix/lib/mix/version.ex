defmodule Mix.Version do
  defexception InvalidRequirement, reason: :invalid_specification do
    def message(InvalidRequirement[reason: reason]) when is_binary(reason) do
      { first, rest } = String.next_grapheme(reason)

      String.downcase(first) <> rest
    end

    def message(InvalidRequirement[]) do
      "invalid version specification"
    end
  end

  defrecord Schema, major: 0, minor: 0, patch: 0, build: nil, source: nil

  import Kernel, except: [match?: 2]

  @doc """
  Checks if the given specification matches the given version.
  """
  @spec match?(String.t | Mix.Version.Requirement.t, String.t | Schema.t) :: boolean
  def match?(spec, version) when is_binary(spec) do
    case Mix.Version.Requirement.parse(spec) do
      { :ok, spec } ->
        match?(spec, version)

      { :error, reason } ->
        raise InvalidRequirement, reason: reason
    end
  end

  def match?(spec, version) when is_binary(version) do
    match?(spec, parse(version))
  end

  def match?(spec, Mix.Version.Schema[] = version) do
    Mix.Version.Requirement.match?(spec, to_matchable(version))
  end

  @doc """
  Checks if a version string is compatible with [semver](http://semver.org/).

  ## Examples

      iex> Mix.Version.valid?("1")
      true
      iex> Mix.Version.valid?("1.0")
      true
      iex> Mix.Version.valid?("1.0.0")
      true
      iex> Mix.Version.valid?("1.0.0+alpha1")
      true
      iex> Mix.Version.valid?("1.0.0-alpha1")
      true
      iex> Mix.Version.valid?("1.0.3.4")
      false

  """
  @spec valid?(String.t | Schema.t) :: boolean
  def valid?(string) when is_binary(string) do
    Regex.match? %r/^\d+(\.\d+(\.\d+)?)?([\-+][^\s]+)?$/, string
  end

  def valid?(Mix.Version.Schema[major: nil]), do: false
  def valid?(Mix.Version.Schema[]),           do: true

  @doc """
  Parse a version into a matchable value.

  ## Examples

      iex> Mix.Version.parse("1")
      1
      iex> Mix.Version.parse("1.0")
      1.0
      iex> Mix.Version.parse("1.0.0")
      1.0.0
      iex> Mix.Version.parse("1.0.0+alpha1")
      1.0.0-alpha1
      iex> Mix.Version.parse("1.0.0-alpha1")
      1.0.0-alpha1
      iex> Mix.Version.parse("1.0.3.4")
      1.0.3.4

  """
  @spec parse(String.t) :: { :ok, Schema.t } | { :error, term }
  def parse(string) when is_binary(string) do
    if valid?(string) do
      destructure [_, major, minor, patch, build],
        Regex.run %r/^(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:[\-+]([^\s]+))?$/, string

      major = binary_to_integer(major)
      minor = binary_to_integer(minor || "0")
      patch = binary_to_integer(patch || "0")
      build = case build && Regex.run(%r/^(.*?)(\d+)?$/, build) do
        [_, build] ->
          { build, 0 }

        [_, build, number] ->
          { build, binary_to_integer(number) }

        nil ->
          nil
      end

      Mix.Version.from_matchable({ major, minor, patch, build })
    else
      Mix.Version.Schema[source: string]
    end
  end

  @doc """
  Get the matchable representation.

  ## Examples

      iex> Mix.Version.to_matchable("1")
      {1,0,0,nil}
      iex> Mix.Version.to_matchable("1.0")
      {1,0,0,nil}
      iex> Mix.Version.to_matchable("1.0.0")
      {1,0,0,nil}
      iex> Mix.Version.to_matchable("1.0.0+alpha1")
      {1,0,0,{"alpha",1}}
      iex> Mix.Version.to_matchable("1.0.0-alpha10")
      {1,0,0,{"alpha",10}}
      iex> Mix.Version.to_matchable("1.0.3.4")
      {"1.0.3.4",nil,nil,nil}

  """
  @spec to_matchable(String.t | Schema.t) :: Mix.Version.Requirement.matchable
  def to_matchable(Schema[major: nil, source: source]) do
    { source, nil, nil, nil }
  end

  def to_matchable(Mix.Version.Schema[major: major, minor: minor, patch: patch, build: nil]) do
    { major, minor, patch, nil }
  end

  def to_matchable(Mix.Version.Schema[major: major, minor: minor, patch: patch, build: build]) do
    build = case Regex.run %r/^(.*?)(\d+)?$/, build do
      [_, build] ->
        { build, 0 }

      [_, build, number] ->
        { build, binary_to_integer(number) }
    end

    { major, minor, patch, build }
  end

  def to_matchable(string) do
    to_matchable(parse(string))
  end

  @doc """
  Convert a matchable to a `Mix.Version`.
  """
  @spec from_matchable(Mix.Version.Requirement.matchable) :: Schema.t
  def from_matchable({ source, nil, nil, nil }) when is_binary(source) do
    Mix.Version.Schema[source: source]
  end

  def from_matchable({ major, minor, patch, build }) do
    source = "#{major}"

    if minor do
      source = "#{source}.#{minor}"

      if patch do
        source = "#{source}.#{patch}"

        if build do
          build  = "#{elem build, 0}#{elem build, 1}"
          source = "#{source}-#{build}"
        end
      end
    end

    Mix.Version.Schema[major: major, minor: minor, patch: patch, build: build, source: source]
  end

  defmodule Requirement.DSL do
    defmacro deflexer(match, do: body) when is_binary(match) do
      quote do
        def lexer(unquote(match) <> rest, acc) do
          lexer(rest, [unquote(body) | acc])
        end
      end
    end

    defmacro deflexer(acc, do: body) do
      quote do
        def lexer("", unquote(acc)) do
          unquote(body)
        end
      end
    end

    defmacro deflexer(char, acc, do: body) do
      quote do
        def lexer(<< unquote(char) :: utf8, rest :: binary >>, unquote(acc)) do
          unquote(char) = << unquote(char) :: utf8 >>

          lexer(rest, unquote(body))
        end
      end
    end
  end

  defmodule Requirement do
    @opaque t :: record

    @type matchable :: { major :: String.t | non_neg_integer,
                         minor :: non_neg_integer | nil,
                         patch :: non_neg_integer | nil,
                         pre   :: { String.t, non_neg_integer } | nil }

    defrecordp :requirement, source: nil, matchspec: nil

    @spec parse(String.t) :: t
    def parse(source) do
      lexed = lexer(source, [])

      if valid?(lexed) do
        spec = to_matchspec(lexed)

        case :ets.test_ms({}, spec) do
          { :ok, _ } ->
            { :ok, requirement(source: source, matchspec: spec) }

          { :error, [error: reason] } ->
            { :error, to_binary(reason) }
        end
      else
        { :error, :invalid_specification }
      end
    end

    @spec match?(t, matchable) :: boolean | no_return
    def match?(requirement(matchspec: spec), version) do
      case :ets.test_ms(version, spec) do
        { :ok, result } ->
          result != false

        { :error, reason } ->
          raise InvalidRequirement, reason: reason
      end
    end

    import Requirement.DSL

    @doc false
    deflexer ">=",    do: :'>='
    deflexer "<=",    do: :'<='
    deflexer "~>",    do: :'~>'
    deflexer ">",     do: :'>'
    deflexer "<",     do: :'<'
    deflexer "==",    do: :'=='
    deflexer "!=",    do: :'!='
    deflexer "!",     do: :'!='
    deflexer " or ",  do: :'||'
    deflexer " and ", do: :'&&'
    deflexer " ",     do: :' '

    deflexer x, [] do
      [x, :'==']
    end

    deflexer x, [h | acc] do
      cond do
        is_binary h ->
          [h <> x | acc]

        h in [:'||', :'&&'] ->
          [x, :'==', h | acc]

        true ->
          [x, h | acc]
      end
    end

    deflexer acc do
      Enum.filter(Enum.reverse(acc), &1 != :' ')
    end

    @doc """
    Checks if the version specification is valid.
    """
    @spec valid?(list) :: boolean
    def valid?([]) do
      false
    end

    def valid?([a | next]) do
      valid?(a, next)
    end

    # it must finish with a version
    defp valid?(a, []) when is_binary(a) do
      true
    end

    # version version
    defp valid?(a, [b | _]) when is_binary(a) and is_binary(b) do
      false
    end

    # or <op> | and <op>
    defp valid?(a, [b | next]) when is_atom(a) and is_atom(b) and a in [:'||', :'&&'] do
      valid?(b, next)
    end

    # <version> or | <version> and
    defp valid?(a, [b | next]) when is_binary(a) and is_atom(b) and b in [:'||', :'&&'] do
      valid?(b, next)
    end

    # or <version> | and <version>
    defp valid?(a, [b | next]) when is_atom(a) and is_binary(b) and a in [:'||', :'&&'] do
      valid?(b, next)
    end

    # <op> <version>; also checks operators work on valid versions
    defp valid?(a, [b | next]) when is_atom(a) and is_binary(b) do
      if Mix.Version.valid?(b) do
        valid?(b, next)
      else
        if a in [:'==', :'!='] and Regex.match? %r/^\w/, b do
          valid?(b, next)
        else
          false
        end
      end
    end

    defp valid?(_, _) do
      false
    end

    defp approximate(version) do
      Mix.Version.from_matchable(case Regex.run %r/^(\d+)(?:\.(\d+)(?:\.(\d+))?)?(?:[\-+]([^\s]+))?$/, version do
        [_, major] ->
          { binary_to_integer(major) + 1, 0, 0, nil }

        [_, major, _] ->
          { binary_to_integer(major) + 1, 0, 0, nil }

        [_, major, minor, _] ->
          { binary_to_integer(major), binary_to_integer(minor) + 1, 0, nil }

        [_, major, minor, patch, _] ->
         { binary_to_integer(major), binary_to_integer(minor), binary_to_integer(patch) + 1, nil }
      end)
    end

    defp to_matchspec(lexed) do
      first = to_condition(lexed)
      rest  = Enum.drop(lexed, 2)

      [{{ :'$1', :'$2', :'$3', :'$4' }, [to_condition(first, rest)], [:'$_'] }]
    end

    defp to_condition([:'==', version | _]) do
      version = Mix.Version.to_matchable(version)

      { :'==', :'$_', { :const, version } }
    end

    defp to_condition([:'!=', version | _]) do
      version = Mix.Version.to_matchable(version)

      { :'/=', :'$_', { :const, version } }
    end

    defp to_condition([:'>', version | _]) do
      version = Mix.Version.to_matchable(version)

      { :andalso, { :not, { :is_binary, :'$1' } },
                  { :'>', :'$_', { :const, version } } }
    end

    defp to_condition([:'>=', version | _]) do
      version = Mix.Version.to_matchable(version)

      { :andalso, { :not, { :is_binary, :'$1' } },
                  { :'>=', :'$_', { :const, version } } }
    end

    defp to_condition([:'<', version | _]) do
      version = Mix.Version.to_matchable(version)

      { :andalso, { :not, { :is_binary, :'$1' } },
                  { :'<', :'$_', { :const, version } } }
    end

    defp to_condition([:'<=', version | _]) do
      version = Mix.Version.to_matchable(version)

      { :andalso, { :not, { :is_binary, :'$1' } },
                  { :'=<', :'$_', { :const, version } } }
    end

    defp to_condition([:'~>', version | _]) do
      from = Mix.Version.parse(version)
      to   = approximate(version)

      { :andalso, to_condition([:'>=', to_binary(from)]),
                  to_condition([:'<', to_binary(to)]) }
    end

    defp to_condition(current, []) do
      current
    end

    defp to_condition(current, [:'&&', operator, version | rest]) do
      to_condition({ :andalso, current, to_condition([operator, version]) }, rest)
    end

    defp to_condition(current, [:'||', operator, version | rest]) do
      to_condition({ :orelse, current, to_condition([operator, version]) }, rest)
    end
  end
end

defimpl Binary.Chars, for: Mix.Version.Schema do
  def to_binary(Mix.Version.Schema[source: source]) do
    source
  end
end

defimpl Binary.Inspect, for: Mix.Version.Schema do
  def inspect(self, _opts) do
    "#Mix.Version.Schema<" <> to_binary(self) <> ">"
  end
end

defimpl Binary.Chars, for: Mix.Version.Requirement do
  def to_binary({ _, source, _ }) do
    source
  end
end

defimpl Binary.Inspect, for: Mix.Version.Requirement do
  def inspect({ _, source, _ }, _opts) do
    "#Mix.Version.Requirement<" <> source <> ">"
  end
end
