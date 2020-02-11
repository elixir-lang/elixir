defmodule Version do
  @moduledoc ~S"""
  Functions for parsing and matching versions against requirements.

  A version is a string in a specific format or a `Version`
  generated after parsing via `Version.parse/1`.

  Although Elixir projects are not required to follow SemVer,
  they must follow the format outlined on [SemVer 2.0 schema](https://semver.org/).

  ## Versions

  In a nutshell, a version is represented by three numbers:

      MAJOR.MINOR.PATCH

  Pre-releases are supported by optionally appending a hyphen and a series of
  period-separated identifiers immediately following the patch version.
  Identifiers consist of only ASCII alphanumeric characters and hyphens (`[0-9A-Za-z-]`):

      "1.0.0-alpha.3"

  Build information can be added by appending a plus sign and a series of
  dot-separated identifiers immediately following the patch or pre-release version.
  Identifiers consist of only ASCII alphanumeric characters and hyphens (`[0-9A-Za-z-]`):

      "1.0.0-alpha.3+20130417140000.amd64"

  ## Struct

  The version is represented by the `Version` struct and fields
  are named according to SemVer 2.0: `:major`, `:minor`, `:patch`,
  `:pre`, and `:build`.

  ## Requirements

  Requirements allow you to specify which versions of a given
  dependency you are willing to work against. Requirements support the common
  comparison operators such as `>`, `>=`, `<`, `<=`, `==`, `!=` that work as one would expect,
  and additionally the special operator `~>` described in detail further below.

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

  `~>` will never include pre-release versions of its upper bound,
  regardless of the usage of the `:allow_pre` option, or whether the operand
  is a pre-release version. It can also be used to set an upper bound on only the major
  version part. See the table below for `~>` requirements and
  their corresponding translations.

  `~>`           | Translation
  :------------- | :---------------------
  `~> 2.0.0`     | `>= 2.0.0 and < 2.1.0`
  `~> 2.1.2`     | `>= 2.1.2 and < 2.2.0`
  `~> 2.1.3-dev` | `>= 2.1.3-dev and < 2.2.0`
  `~> 2.0`       | `>= 2.0.0 and < 3.0.0`
  `~> 2.1`       | `>= 2.1.0 and < 3.0.0`

  The requirement operand after the `~>` is allowed to omit the patch version,
  allowing us to express `~> 2.1` or `~> 2.1-dev`, something that wouldn't be allowed
  when using the common comparison operators.

  When the `:allow_pre` option is set `false` in `Version.match?/3`, the requirement
  will not match a pre-release version unless the operand is a pre-release version.
  The default is to always allow pre-releases but note that in
  Hex `:allow_pre` is set to `false`. See the table below for examples.

  Requirement    | Version     | `:allow_pre`      | Matches
  :------------- | :---------- | :---------------- | :------
  `~> 2.0`       | `2.1.0`     | `true` or `false` | `true`
  `~> 2.0`       | `3.0.0`     | `true` or `false` | `false`
  `~> 2.0.0`     | `2.0.5`     | `true` or `false` | `true`
  `~> 2.0.0`     | `2.1.0`     | `true` or `false` | `false`
  `~> 2.1.2`     | `2.1.6-dev` | `true`            | `true`
  `~> 2.1.2`     | `2.1.6-dev` | `false`           | `false`
  `~> 2.1-dev`   | `2.2.0-dev` | `true` or `false` | `true`
  `~> 2.1.2-dev` | `2.1.6-dev` | `true` or `false` | `true`
  `>= 2.1.0`     | `2.2.0-dev` | `true`            | `true`
  `>= 2.1.0`     | `2.2.0-dev` | `false`           | `false`
  `>= 2.1.0-dev` | `2.2.6-dev` | `true` or `false` | `true`

  """

  import Kernel, except: [match?: 2]
  defstruct [:major, :minor, :patch, :pre, :build]

  @type version :: String.t() | t
  @type requirement :: String.t() | Version.Requirement.t()
  @type major :: non_neg_integer
  @type minor :: non_neg_integer
  @type patch :: non_neg_integer
  @type pre :: [String.t() | non_neg_integer]
  @type build :: String.t() | nil
  @type t :: %__MODULE__{major: major, minor: minor, patch: patch, pre: pre, build: build}

  defmodule Requirement do
    @moduledoc """
    A struct that holds version requirement information.

    The struct fields are private and should not be accessed.

    See the "Requirements" section in the `Version` module
    for more information.
    """

    defstruct [:source, :lexed]

    @opaque t :: %__MODULE__{
              source: String.t(),
              lexed: [atom | matchable]
            }

    @typep matchable ::
             {Version.major(), Version.minor(), Version.patch(), Version.pre(), Version.build()}

    @compile inline: [compare: 2]

    @doc false
    @spec new(String.t(), :ets.match_spec()) :: t
    def new(source, lexed) do
      %__MODULE__{source: source, lexed: lexed}
    end

    @doc false
    @spec match?(t, tuple) :: boolean
    def match?(%__MODULE__{lexed: lexed}, matchable_pattern) do
      match_lexed?(lexed, matchable_pattern)
    end

    defp match_lexed?([operator, req, :&& | rest], version) do
      match_op?(operator, req, version) and match_lexed?(rest, version)
    end

    defp match_lexed?([operator, req, :|| | rest], version) do
      match_op?(operator, req, version) or match_lexed?(rest, version)
    end

    defp match_lexed?([operator, req], version) do
      match_op?(operator, req, version)
    end

    defp match_op?(:==, req, version) do
      compare(version, req) == :eq
    end

    defp match_op?(:!=, req, version) do
      compare(version, req) != :eq
    end

    defp match_op?(:~>, {major, minor, nil, req_pre, _}, {_, _, _, pre, allow_pre} = version) do
      compare(version, {major, minor, 0, req_pre, nil}) in [:eq, :gt] and
        compare(version, {major + 1, 0, 0, [0], nil}) == :lt and
        (allow_pre or req_pre != [] or pre == [])
    end

    defp match_op?(:~>, {major, minor, _, req_pre, _} = req, {_, _, _, pre, allow_pre} = version) do
      compare(version, req) in [:eq, :gt] and
        compare(version, {major, minor + 1, 0, [0], nil}) == :lt and
        (allow_pre or req_pre != [] or pre == [])
    end

    defp match_op?(:>, {_, _, _, req_pre, _} = req, {_, _, _, pre, allow_pre} = version) do
      compare(version, req) == :gt and (allow_pre or req_pre != [] or pre == [])
    end

    defp match_op?(:>=, {_, _, _, req_pre, _} = req, {_, _, _, pre, allow_pre} = version) do
      compare(version, req) in [:eq, :gt] and (allow_pre or req_pre != [] or pre == [])
    end

    defp match_op?(:<, req, version) do
      compare(version, req) == :lt
    end

    defp match_op?(:<=, req, version) do
      compare(version, req) in [:eq, :lt]
    end

    defp compare({major1, minor1, patch1, pre1, _}, {major2, minor2, patch2, pre2, _}) do
      cond do
        major1 > major2 -> :gt
        major1 < major2 -> :lt
        minor1 > minor2 -> :gt
        minor1 < minor2 -> :lt
        patch1 > patch2 -> :gt
        patch1 < patch2 -> :lt
        pre1 == [] and pre2 != [] -> :gt
        pre1 != [] and pre2 == [] -> :lt
        pre1 > pre2 -> :gt
        pre1 < pre2 -> :lt
        true -> :eq
      end
    end
  end

  defmodule InvalidRequirementError do
    defexception [:requirement]

    @impl true
    def exception(requirement) when is_binary(requirement) do
      %__MODULE__{requirement: requirement}
    end

    @impl true
    def message(%{requirement: requirement}) do
      "invalid requirement: #{inspect(requirement)}"
    end
  end

  defmodule InvalidVersionError do
    defexception [:version]

    @impl true
    def exception(version) when is_binary(version) do
      %__MODULE__{version: version}
    end

    @impl true
    def message(%{version: version}) do
      "invalid version: #{inspect(version)}"
    end
  end

  @doc """
  Checks if the given version matches the specification.

  Returns `true` if `version` satisfies `requirement`, `false` otherwise.
  Raises a `Version.InvalidRequirementError` exception if `requirement` is not
  parsable, or a `Version.InvalidVersionError` exception if `version` is not parsable.
  If given an already parsed version and requirement this function won't
  raise.

  ## Options

    * `:allow_pre` (boolean) - when `false`, pre-release versions will not match
      unless the operand is a pre-release version. Defaults to `true`.
      For examples, please refer to the table above under the "Requirements" section.

  ## Examples

      iex> Version.match?("2.0.0", "> 1.0.0")
      true

      iex> Version.match?("2.0.0", "== 1.0.0")
      false

      iex> Version.match?("2.1.6-dev", "~> 2.1.2")
      true

      iex> Version.match?("2.1.6-dev", "~> 2.1.2", allow_pre: false)
      false

      iex> Version.match?("foo", "== 1.0.0")
      ** (Version.InvalidVersionError) invalid version: "foo"

      iex> Version.match?("2.0.0", "== == 1.0.0")
      ** (Version.InvalidRequirementError) invalid requirement: "== == 1.0.0"

  """
  @spec match?(version, requirement, keyword) :: boolean
  def match?(version, requirement, opts \\ [])

  def match?(version, requirement, opts) when is_binary(requirement) do
    match?(version, parse_requirement!(requirement), opts)
  end

  def match?(version, requirement, opts) do
    allow_pre = Keyword.get(opts, :allow_pre, true)
    matchable_pattern = to_matchable(version, allow_pre)

    Requirement.match?(requirement, matchable_pattern)
  end

  @doc """
  Compares two versions.

  Returns `:gt` if the first version is greater than the second one, and `:lt`
  for vice versa. If the two versions are equal, `:eq` is returned.

  Pre-releases are strictly less than their corresponding release versions.

  Patch segments are compared lexicographically if they are alphanumeric, and
  numerically otherwise.

  Build segments are ignored: if two versions differ only in their build segment
  they are considered to be equal.

  Raises a `Version.InvalidVersionError` exception if any of the two given
  versions are not parsable. If given an already parsed version this function
  won't raise.

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
      ** (Version.InvalidVersionError) invalid version: "invalid"

  """
  @spec compare(version, version) :: :gt | :eq | :lt
  def compare(version1, version2) do
    do_compare(to_matchable(version1, true), to_matchable(version2, true))
  end

  defp do_compare({major1, minor1, patch1, pre1, _}, {major2, minor2, patch2, pre2, _}) do
    cond do
      major1 > major2 -> :gt
      major1 < major2 -> :lt
      minor1 > minor2 -> :gt
      minor1 < minor2 -> :lt
      patch1 > patch2 -> :gt
      patch1 < patch2 -> :lt
      pre1 == [] and pre2 != [] -> :gt
      pre1 != [] and pre2 == [] -> :lt
      pre1 > pre2 -> :gt
      pre1 < pre2 -> :lt
      true -> :eq
    end
  end

  @doc """
  Parses a version string into a `Version` struct.

  ## Examples

      iex> {:ok, version} = Version.parse("2.0.1-alpha1")
      iex> version
      #Version<2.0.1-alpha1>

      iex> Version.parse("2.0-alpha1")
      :error

  """
  @spec parse(String.t()) :: {:ok, t} | :error
  def parse(string) when is_binary(string) do
    case Version.Parser.parse_version(string) do
      {:ok, {major, minor, patch, pre, build_parts}} ->
        build = if build_parts == [], do: nil, else: Enum.join(build_parts, "")
        version = %Version{major: major, minor: minor, patch: patch, pre: pre, build: build}
        {:ok, version}

      :error ->
        :error
    end
  end

  @doc """
  Parses a version string into a `Version`.

  If `string` is an invalid version, a `Version.InvalidVersionError` is raised.

  ## Examples

      iex> Version.parse!("2.0.1-alpha1")
      #Version<2.0.1-alpha1>

      iex> Version.parse!("2.0-alpha1")
      ** (Version.InvalidVersionError) invalid version: "2.0-alpha1"

  """
  @spec parse!(String.t()) :: t
  def parse!(string) when is_binary(string) do
    case parse(string) do
      {:ok, version} -> version
      :error -> raise InvalidVersionError, string
    end
  end

  @doc """
  Parses a version requirement string into a `Version.Requirement` struct.

  ## Examples

      iex> {:ok, requirement} = Version.parse_requirement("== 2.0.1")
      iex> requirement
      #Version.Requirement<== 2.0.1>

      iex> Version.parse_requirement("== == 2.0.1")
      :error

  """
  @spec parse_requirement(String.t()) :: {:ok, Requirement.t()} | :error
  def parse_requirement(string) when is_binary(string) do
    case Version.Parser.parse_requirement(string) do
      {:ok, lexed} -> {:ok, Requirement.new(string, lexed)}
      :error -> :error
    end
  end

  @doc """
  Parses a version requirement string into a `Version.Requirement` struct.

  If `string` is an invalid requirement, a `Version.InvalidRequirementError` is raised.

  ## Examples

      iex> Version.parse_requirement!("== 2.0.1")
      #Version.Requirement<== 2.0.1>

      iex> Version.parse_requirement!("== == 2.0.1")
      ** (Version.InvalidRequirementError) invalid requirement: "== == 2.0.1"

  """
  @doc since: "1.8.0"
  @spec parse_requirement!(String.t()) :: Requirement.t()
  def parse_requirement!(string) when is_binary(string) do
    case parse_requirement(string) do
      {:ok, requirement} -> requirement
      :error -> raise InvalidRequirementError, string
    end
  end

  @doc """
  Compiles a requirement to an internal representation that may optimize matching.

  The internal representation is opaque.
  """
  @spec compile_requirement(Requirement.t()) :: Requirement.t()
  def compile_requirement(requirement) do
    requirement
  end

  defp to_matchable(%Version{major: major, minor: minor, patch: patch, pre: pre}, allow_pre?) do
    {major, minor, patch, pre, allow_pre?}
  end

  defp to_matchable(string, allow_pre?) do
    case Version.Parser.parse_version(string) do
      {:ok, {major, minor, patch, pre, _build_parts}} ->
        {major, minor, patch, pre, allow_pre?}

      :error ->
        raise InvalidVersionError, string
    end
  end

  defmodule Parser do
    @moduledoc false

    operators = [
      {">=", :>=},
      {"<=", :<=},
      {"~>", :~>},
      {">", :>},
      {"<", :<},
      {"==", :==},
      {"!=", :!=},
      {"!", :!=},
      {" or ", :||},
      {" and ", :&&}
    ]

    for {string_op, atom_op} <- operators do
      def lexer(unquote(string_op) <> rest, acc) do
        lexer(rest, [unquote(atom_op) | acc])
      end
    end

    def lexer(" " <> rest, acc) do
      lexer(rest, acc)
    end

    def lexer(<<char::utf8, rest::binary>>, []) do
      lexer(rest, [<<char::utf8>>, :==])
    end

    def lexer(<<char::utf8, body::binary>>, [head | acc]) do
      acc =
        case head do
          head when is_binary(head) ->
            [<<head::binary, char::utf8>> | acc]

          head when head in [:||, :&&] ->
            [<<char::utf8>>, :==, head | acc]

          _other ->
            [<<char::utf8>>, head | acc]
        end

      lexer(body, acc)
    end

    def lexer("", acc) do
      Enum.map(Enum.reverse(acc), fn
        op when is_atom(op) ->
          op

        version when is_binary(version) ->
          case Version.Parser.parse_version(version, true) do
            {:ok, version} -> version
            :error -> :error
          end
      end)
    end

    @spec parse_requirement(String.t()) :: {:ok, term} | :error
    def parse_requirement(source) do
      lexed = lexer(source, [])

      if valid_requirement?(lexed) do
        {:ok, lexed}
      else
        :error
      end
    end

    def parse_version(string, approximate? \\ false) when is_binary(string) do
      destructure [version_with_pre, build], String.split(string, "+", parts: 2)
      destructure [version, pre], String.split(version_with_pre, "-", parts: 2)
      destructure [major, minor, patch, next], String.split(version, ".")

      with nil <- next,
           {:ok, major} <- require_digits(major),
           {:ok, minor} <- require_digits(minor),
           {:ok, patch} <- maybe_patch(patch, approximate?),
           {:ok, pre_parts} <- optional_dot_separated(pre),
           {:ok, pre_parts} <- convert_parts_to_integer(pre_parts, []),
           {:ok, build_parts} <- optional_dot_separated(build) do
        {:ok, {major, minor, patch, pre_parts, build_parts}}
      else
        _other -> :error
      end
    end

    defp require_digits(nil), do: :error

    defp require_digits(string) do
      if leading_zero?(string), do: :error, else: parse_digits(string, "")
    end

    defp leading_zero?(<<?0, _, _::binary>>), do: true
    defp leading_zero?(_), do: false

    defp parse_digits(<<char, rest::binary>>, acc) when char in ?0..?9,
      do: parse_digits(rest, <<acc::binary, char>>)

    defp parse_digits(<<>>, acc) when byte_size(acc) > 0, do: {:ok, String.to_integer(acc)}
    defp parse_digits(_, _acc), do: :error

    defp maybe_patch(patch, approximate?)
    defp maybe_patch(nil, true), do: {:ok, nil}
    defp maybe_patch(patch, _), do: require_digits(patch)

    defp optional_dot_separated(nil), do: {:ok, []}

    defp optional_dot_separated(string) do
      parts = String.split(string, ".")

      if Enum.all?(parts, &(&1 != "" and valid_identifier?(&1))) do
        {:ok, parts}
      else
        :error
      end
    end

    defp convert_parts_to_integer([part | rest], acc) do
      case parse_digits(part, "") do
        {:ok, integer} ->
          if leading_zero?(part) do
            :error
          else
            convert_parts_to_integer(rest, [integer | acc])
          end

        :error ->
          convert_parts_to_integer(rest, [part | acc])
      end
    end

    defp convert_parts_to_integer([], acc) do
      {:ok, Enum.reverse(acc)}
    end

    defp valid_identifier?(<<char, rest::binary>>)
         when char in ?0..?9
         when char in ?a..?z
         when char in ?A..?Z
         when char == ?- do
      valid_identifier?(rest)
    end

    defp valid_identifier?(<<>>) do
      true
    end

    defp valid_identifier?(_other) do
      false
    end

    defp valid_requirement?([]), do: false
    defp valid_requirement?([a | next]), do: valid_requirement?(a, next)

    # it must finish with a version
    defp valid_requirement?(a, []) when is_tuple(a) do
      true
    end

    # or <op> | and <op>
    defp valid_requirement?(a, [b | next]) when is_atom(a) and is_atom(b) and a in [:||, :&&] do
      valid_requirement?(b, next)
    end

    # <version> or | <version> and
    defp valid_requirement?(a, [b | next]) when is_tuple(a) and is_atom(b) and b in [:||, :&&] do
      valid_requirement?(b, next)
    end

    # or <version> | and <version>
    defp valid_requirement?(a, [b | next]) when is_atom(a) and is_tuple(b) and a in [:||, :&&] do
      valid_requirement?(b, next)
    end

    # ~> <version>
    defp valid_requirement?(:~>, [b | next]) when is_tuple(b) do
      valid_requirement?(b, next)
    end

    # <op> <version>
    defp valid_requirement?(a, [{_major, _minor, patch, _pre, _build} = b | next])
         when is_atom(a) and is_integer(patch) do
      valid_requirement?(b, next)
    end

    defp valid_requirement?(_, _) do
      false
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
