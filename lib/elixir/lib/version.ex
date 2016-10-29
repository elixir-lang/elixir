defmodule Version do
  @moduledoc ~S"""
  Functions for parsing and matching versions against requirements.

  A version is a string in a specific format or a `Version`
  generated after parsing via `Version.parse/1`.

  `Version` parsing and requirements follow
  [SemVer 2.0 schema](http://semver.org/).

  ## Versions

  In a nutshell, a version is represented by three numbers:

      MAJOR.MINOR.PATCH

  Pre-releases are supported by appending `-[0-9A-Za-z-\.]`:

      "1.0.0-alpha.3"

  Build information can be added by appending `+[0-9A-Za-z-\.]`:

      "1.0.0-alpha.3+20130417140000"

  ## Struct

  The version is represented by the Version struct and fields
  are named according to Semver: `:major`, `:minor`, `:patch`,
  `:pre` and `:build`.

  ## Requirements

  Requirements allow you to specify which versions of a given
  dependency you are willing to work against. It supports common
  operators like `>=`, `<=`, `>`, `==` and friends that
  work as one would expect:

      # Only version 2.0.0
      "== 2.0.0"

      # Anything later than 2.0.0
      "> 2.0.0"

  Requirements also support `and` and `or` for complex conditions:

      # 2.0.0 and later until 2.1.0
      ">= 2.0.0 and < 2.1.0"

  Since the example above is such a common requirement, it can
  be expressed as:

      "~> 2.0.0"

  `~>` will never include pre-release versions of its upper bound.
  It can also be used to set an upper bound on only the major
  version part. See the table below for `~>` requirements and
  their corresponding translation.

  `~>`           | Translation
  :------------- | :---------------------
  `~> 2.0.0`     | `>= 2.0.0 and < 2.1.0`
  `~> 2.1.2`     | `>= 2.1.2 and < 2.2.0`
  `~> 2.1.3-dev` | `>= 2.1.3-dev and < 2.2.0`
  `~> 2.0`       | `>= 2.0.0 and < 3.0.0`
  `~> 2.1`       | `>= 2.1.0 and < 3.0.0`

  When `allow_pre: false` is set the requirement will not match a
  pre-release version unless the operand is a pre-release version.
  The default is to allow always allow pre-releases but note that in
  Hex `:allow_pre` is set to `false.` See the table below for examples.

  Requirement    | Version     | `:allow_pre` | Matches
  :------------- | :---------- | :----------- | :------
  `~> 2.0`       | `2.1.0`     | -            | `true`
  `~> 2.0`       | `3.0.0`     | -            | `false`
  `~> 2.0.0`     | `2.0.1`     | -            | `true`
  `~> 2.0.0`     | `2.1.0`     | -            | `false`
  `~> 2.1.2`     | `2.1.3-dev` | `true`       | `true`
  `~> 2.1.2`     | `2.1.3-dev` | `false`      | `false`
  `~> 2.1-dev`   | `2.2.0-dev` | `false`      | `true`
  `~> 2.1.2-dev` | `2.1.3-dev` | `false`      | `true`
  `>= 2.1.0`     | `2.2.0-dev` | `false`      | `false`
  `>= 2.1.0-dev` | `2.2.3-dev` | `true`       | `true`

  """

  import Kernel, except: [match?: 2]
  defstruct [:major, :minor, :patch, :pre, :build]

  @type version     :: String.t | t
  @type requirement :: String.t | Version.Requirement.t
  @type major       :: String.t | non_neg_integer
  @type minor       :: non_neg_integer | nil
  @type patch       :: non_neg_integer | nil
  @type pre         :: [String.t | non_neg_integer]
  @type build       :: String.t | nil
  @type matchable   :: {major :: major,
                        minor :: minor,
                        patch :: patch,
                        pre   :: pre}
  @type t           :: %__MODULE__{
                         major: major,
                         minor: minor,
                         patch: patch,
                         pre:   pre,
                         build: build}

  defmodule Requirement do
    defstruct [:source, :matchspec, :compiled]
    @type t :: %__MODULE__{}
  end

  defmodule InvalidRequirementError do
    defexception [:message]
  end

  defmodule InvalidVersionError do
    defexception [:message]
  end

  @doc """
  Checks if the given version matches the specification.

  Returns `true` if `version` satisfies `requirement`, `false` otherwise.
  Raises a `Version.InvalidRequirementError` exception if `requirement` is not
  parsable, or `Version.InvalidVersionError` if `version` is not parsable.
  If given an already parsed version and requirement this function won't
  raise.

  ## Options

    * `:allow_pre` - when `false` pre-release versions will not match
      unless the operand is a pre-release version, see the table above
      for examples  (default: `true`);

  ## Examples

      iex> Version.match?("2.0.0", ">1.0.0")
      true

      iex> Version.match?("2.0.0", "==1.0.0")
      false

      iex> Version.match?("foo", "==1.0.0")
      ** (Version.InvalidVersionError) foo

      iex> Version.match?("2.0.0", "== ==1.0.0")
      ** (Version.InvalidRequirementError) == ==1.0.0

  """
  @spec match?(version, requirement, Keyword.t) :: boolean
  def match?(version, requirement, opts \\ [])

  def match?(version, requirement, opts) when is_binary(requirement) do
    case parse_requirement(requirement) do
      {:ok, requirement} ->
        match?(version, requirement, opts)
      :error ->
        raise InvalidRequirementError, message: requirement
    end
  end

  def match?(version, %Requirement{matchspec: spec, compiled: false}, opts) do
    allow_pre = Keyword.get(opts, :allow_pre, true)
    {:ok, result} = :ets.test_ms(to_matchable(version, allow_pre), spec)
    result != false
  end

  def match?(version, %Requirement{matchspec: spec, compiled: true}, opts) do
    allow_pre = Keyword.get(opts, :allow_pre, true)
    :ets.match_spec_run([to_matchable(version, allow_pre)], spec) != []
  end

  @doc """
  Compares two versions. Returns `:gt` if the first version is greater than
  the second one, and `:lt` for vice versa. If the two versions are equal `:eq`
  is returned.

  Pre-releases are strictly less than their corresponding release versions.

  Patch segments are compared lexicographically if they are alphanumeric, and
  numerically otherwise.

  Build segments are ignored, if two versions differ only in their build segment
  they are considered to be equal.

  Raises a `Version.InvalidVersionError` exception if any of the two are not
  parsable. If given an already parsed version this function won't raise.

  ## Examples

      iex> Version.compare("2.0.1-alpha1", "2.0.0")
      :gt

      iex> Version.compare("1.0.0-beta", "1.0.0-rc1")
      :lt

      iex> Version.compare("1.0.0-10", "1.0.0-2")
      :gt

      iex> Version.compare("2.0.1+build0", "2.0.1")
      :eq

      iex> Version.compare("invalid", "2.0.1")
      ** (Version.InvalidVersionError) invalid

  """
  @spec compare(version, version) :: :gt | :eq | :lt
  def compare(version1, version2) do
    do_compare(to_matchable(version1, true), to_matchable(version2, true))
  end

  defp do_compare({major1, minor1, patch1, pre1, _}, {major2, minor2, patch2, pre2, _}) do
    cond do
      {major1, minor1, patch1} > {major2, minor2, patch2} -> :gt
      {major1, minor1, patch1} < {major2, minor2, patch2} -> :lt
      pre1 == [] and pre2 != [] -> :gt
      pre1 != [] and pre2 == [] -> :lt
      pre1 > pre2 -> :gt
      pre1 < pre2 -> :lt
      true -> :eq
    end
  end

  @doc """
  Parses a version string into a `Version`.

  ## Examples

      iex> {:ok, version} = Version.parse("2.0.1-alpha1")
      iex> version
      #Version<2.0.1-alpha1>

      iex> Version.parse("2.0-alpha1")
      :error

  """
  @spec parse(String.t) :: {:ok, t} | :error
  def parse(string) when is_binary(string) do
    case Version.Parser.parse_version(string) do
      {:ok, {major, minor, patch, pre}} ->
        version = %Version{major: major, minor: minor, patch: patch,
                       pre: pre, build: get_build(string)}
        {:ok, version}
     :error ->
       :error
    end
  end

  @doc """
  Parses a version string into a `Version`.

  If `string` is an invalid version, an `InvalidVersionError` is raised.

  ## Examples

      iex> Version.parse!("2.0.1-alpha1")
      #Version<2.0.1-alpha1>

      iex> Version.parse!("2.0-alpha1")
      ** (Version.InvalidVersionError) 2.0-alpha1

  """
  @spec parse!(String.t) :: t | no_return
  def parse!(string) when is_binary(string) do
    case parse(string) do
      {:ok, version} -> version
      :error -> raise InvalidVersionError, message: string
    end
  end

  @doc """
  Parses a version requirement string into a `Version.Requirement`.

  ## Examples

      iex> {:ok, req} = Version.parse_requirement("== 2.0.1")
      iex> req
      #Version.Requirement<== 2.0.1>

      iex> Version.parse_requirement("== == 2.0.1")
      :error

  """
  @spec parse_requirement(String.t) :: {:ok, Requirement.t} | :error
  def parse_requirement(string) when is_binary(string) do
    case Version.Parser.parse_requirement(string) do
      {:ok, spec} ->
        {:ok, %Requirement{source: string, matchspec: spec, compiled: false}}
      :error ->
        :error
    end
  end

  @doc """
  Compiles a requirement to its internal representation with
  `:ets.match_spec_compile/1` for faster matching.

  The internal representation is opaque and can not be converted to external
  term format and then back again without losing its properties (meaning it
  can not be sent to a process on another node and still remain a valid
  compiled match_spec, nor can it be stored on disk).
  """
  @spec compile_requirement(Requirement.t) :: Requirement.t
  def compile_requirement(%Requirement{matchspec: spec} = req) do
    %{req | matchspec: :ets.match_spec_compile(spec), compiled: true}
  end

  defp to_matchable(%Version{major: major, minor: minor, patch: patch, pre: pre}, allow_pre?) do
    {major, minor, patch, pre, allow_pre?}
  end

  defp to_matchable(string, allow_pre?) do
    case Version.Parser.parse_version(string) do
      {:ok, {major, minor, patch, pre}} ->
        {major, minor, patch, pre, allow_pre?}
      :error ->
        raise InvalidVersionError, message: string
    end
  end

  defp get_build(string) do
    case Regex.run(~r/\+([^\s]+)$/, string) do
      nil ->
        nil

      [_, build] ->
        build
    end
  end

  defmodule Parser.DSL do
    @moduledoc false

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
        def lexer(<<unquote(char)::utf8, rest::binary>>, unquote(acc)) do
          unquote(char) = <<unquote(char)::utf8>>

          lexer(rest, unquote(body))
        end
      end
    end
  end

  defmodule Parser do
    @moduledoc false
    import Parser.DSL

    deflexer ">=",    do: :>=
    deflexer "<=",    do: :<=
    deflexer "~>",    do: :~>
    deflexer ">",     do: :>
    deflexer "<",     do: :<
    deflexer "==",    do: :==
    deflexer "!=",    do: :!=
    deflexer "!",     do: :!=
    deflexer " or ",  do: :||
    deflexer " and ", do: :&&
    deflexer " ",     do: :' '

    deflexer x, [] do
      [x, :'==']
    end

    deflexer x, [h | acc] do
      cond do
        is_binary h ->
          [h <> x | acc]

        h in [:||, :&&] ->
          [x, :==, h | acc]

        true ->
          [x, h | acc]
      end
    end

    deflexer acc do
      Enum.filter(Enum.reverse(acc), &(&1 != :' '))
    end

    @version_regex ~r/^
      (\d+)                 # major
      (?:\.(\d+))?          # minor
      (?:\.(\d+))?          # patch
      (?:\-([\d\w\.\-]+))?  # pre
      (?:\+([\d\w\.\-]+))?  # build
      $/x

    @spec parse_requirement(String.t) :: {:ok, term} | :error
    def parse_requirement(source) do
      lexed = lexer(source, [])
      to_matchspec(lexed)
    end

    defp nillify(""), do: nil
    defp nillify(o),  do: o

    @spec parse_version(String.t) :: {:ok, Version.matchable} | :error
    def parse_version(string, approximate? \\ false) when is_binary(string) do
      if parsed = Regex.run(@version_regex, string) do
        destructure [_, major, minor, patch, pre], parsed
        patch = nillify(patch)
        pre   = nillify(pre)

        if is_nil(minor) or (is_nil(patch) and not approximate?) do
          :error
        else
          major = String.to_integer(major)
          minor = String.to_integer(minor)
          patch = patch && String.to_integer(patch)

          case parse_pre(pre) do
            {:ok, pre} ->
              {:ok, {major, minor, patch, pre}}
            :error ->
              :error
          end
        end
      else
        :error
      end
    end

    defp parse_pre(nil), do: {:ok, []}
    defp parse_pre(pre), do: parse_pre(String.split(pre, "."), [])

    defp parse_pre([piece | t], acc) do
      cond do
        piece =~ ~r/^(0|[1-9][0-9]*)$/ ->
          parse_pre(t, [String.to_integer(piece) | acc])
        piece =~ ~r/^[0-9]*$/ ->
          :error
        true ->
          parse_pre(t, [piece | acc])
      end
    end

    defp parse_pre([], acc) do
      {:ok, Enum.reverse(acc)}
    end

    defp valid_requirement?([]), do: false
    defp valid_requirement?([a | next]), do: valid_requirement?(a, next)

    # it must finish with a version
    defp valid_requirement?(a, []) when is_binary(a) do
      true
    end

    # or <op> | and <op>
    defp valid_requirement?(a, [b | next]) when is_atom(a) and is_atom(b) and a in [:'||', :'&&'] do
      valid_requirement?(b, next)
    end

    # <version> or | <version> and
    defp valid_requirement?(a, [b | next]) when is_binary(a) and is_atom(b) and b in [:'||', :'&&'] do
      valid_requirement?(b, next)
    end

    # or <version> | and <version>
    defp valid_requirement?(a, [b | next]) when is_atom(a) and is_binary(b) and a in [:'||', :'&&'] do
      valid_requirement?(b, next)
    end

    # <op> <version>
    defp valid_requirement?(a, [b | next]) when is_atom(a) and is_binary(b) do
      valid_requirement?(b, next)
    end

    defp valid_requirement?(_, _) do
      false
    end

    defp approximate_upper(version) do
      case version do
        {major, _minor, nil, _} ->
          {major + 1, 0, 0, [0]}

        {major, minor, _patch, _} ->
          {major, minor + 1, 0, [0]}
      end
    end

    defp to_matchspec(lexed) do
      if valid_requirement?(lexed) do
        first = to_condition(lexed)
        rest  = Enum.drop(lexed, 2)
        {:ok, [{{:'$1', :'$2', :'$3', :'$4', :'$5'}, [to_condition(first, rest)], [:'$_']}]}
      else
        :error
      end
    catch
      :invalid_matchspec -> :error
    end

    defp to_condition([:==, version | _]) do
      matchable = parse_condition(version)
      main_condition(:==, matchable)
    end

    defp to_condition([:!=, version | _]) do
      matchable = parse_condition(version)
      main_condition(:'/=', matchable)
    end

    defp to_condition([:~>, version | _]) do
      from = parse_condition(version, true)
      to   = approximate_upper(from)

      {:andalso, to_condition([:>=, matchable_to_string(from)]),
                 to_condition([:<, matchable_to_string(to)])}
    end

    defp to_condition([:>, version | _]) do
      {major, minor, patch, pre} = parse_condition(version)

      {:andalso, {:orelse, main_condition(:>, {major, minor, patch}),
                           {:andalso, main_condition(:==, {major, minor, patch}),
                                      pre_condition(:>, pre)}},
                  no_pre_condition(pre)}
    end

    defp to_condition([:>=, version | _]) do
      matchable = parse_condition(version)

      {:orelse, main_condition(:==, matchable),
                to_condition([:>, version])}
    end

    defp to_condition([:<, version | _]) do
      {major, minor, patch, pre} = parse_condition(version)

      {:orelse, main_condition(:<, {major, minor, patch}),
                {:andalso, main_condition(:==, {major, minor, patch}),
                           pre_condition(:<, pre)}}
    end

    defp to_condition([:<=, version | _]) do
      matchable = parse_condition(version)

      {:orelse, main_condition(:==, matchable),
                to_condition([:<, version])}
    end

    defp to_condition(current, []) do
      current
    end

    defp to_condition(current, [:&&, operator, version | rest]) do
      to_condition({:andalso, current, to_condition([operator, version])}, rest)
    end

    defp to_condition(current, [:||, operator, version | rest]) do
      to_condition({:orelse, current, to_condition([operator, version])}, rest)
    end

    defp parse_condition(version, approximate? \\ false) do
      case parse_version(version, approximate?) do
        {:ok, version} -> version
        :error -> throw :invalid_matchspec
      end
    end

    defp main_condition(op, version) when tuple_size(version) == 3 do
      {op, {{:'$1', :'$2', :'$3'}},
           {:const, version}}
    end

    defp main_condition(op, version) when tuple_size(version) == 4 do
      {op, {{:'$1', :'$2', :'$3', :'$4'}},
           {:const, version}}
    end

    defp pre_condition(:>, pre) do
      length_pre = length(pre)

      {:orelse, {:andalso, {:==, {:length, :'$4'}, 0},
                           {:const, length_pre != 0}},
                {:andalso, {:const, length_pre != 0},
                           {:orelse, {:>, {:length, :'$4'}, length_pre},
                                     {:andalso, {:==, {:length, :'$4'}, length_pre},
                                                {:>, :'$4', {:const, pre}}}}}}
    end

    defp pre_condition(:<, pre) do
      length_pre = length(pre)

      {:orelse, {:andalso, {:'/=', {:length, :'$4'}, 0},
                           {:const, length_pre == 0}},
                {:andalso, {:'/=', {:length, :'$4'}, 0},
                           {:orelse, {:<, {:length, :'$4'}, length_pre},
                                     {:andalso, {:==, {:length, :'$4'}, length_pre},
                                                {:<, :'$4', {:const, pre}}}}}}
    end

    defp no_pre_condition([]) do
      {:orelse, :'$5', {:==, {:length, :'$4'}, 0}}
    end
    defp no_pre_condition(_pre) do
      {:const, true}
    end

    defp matchable_to_string({major, minor, patch, pre}) do
      patch = if patch, do: "#{patch}", else: "0"
      pre   = if pre != [], do: "-#{Enum.join(pre, ".")}"
      "#{major}.#{minor}.#{patch}#{pre}"
    end
  end
end

defimpl String.Chars, for: Version do
  def to_string(version) do
    pre = pre(version.pre)
    build = if build = version.build, do: "+#{build}"
    "#{version.major}.#{version.minor}.#{version.patch}#{pre}#{build}"
  end

  defp pre([]) do
    ""
  end

  defp pre(pre) do
    "-" <>
      Enum.map_join(pre, ".", fn
        int when is_integer(int) -> Integer.to_string(int)
        string when is_binary(string) -> string
      end)
  end
end

defimpl Inspect, for: Version do
  def inspect(self, _opts) do
    "#Version<" <> to_string(self) <> ">"
  end
end

defimpl String.Chars, for: Version.Requirement do
  def to_string(%Version.Requirement{source: source}) do
    source
  end
end

defimpl Inspect, for: Version.Requirement do
  def inspect(%Version.Requirement{source: source}, _opts) do
    "#Version.Requirement<" <> source <> ">"
  end
end
