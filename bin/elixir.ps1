$MinPowerShellVersion = [semver]::new(7, 2, 0)

if ($MinPowerShellVersion.CompareTo($PSVersionTable.PSVersion) -eq 1) {
  Write-Error "This script requires PowerShell version 7.2 or above. Running on $($PSVersionTable.PSVersion)"
}

$ELIXIR_VERSION = "1.17.0-dev"
$ScriptPath = Split-Path -Parent $PSCommandPath
$ErlExec = "erl"

function PrintElixirHelp {
  $ScriptName = Split-Path -Leaf $PSCommandPath
  $Help = @"
Usage: $ScriptName [options] [.exs file] [data]

## General options

  -e "COMMAND"                 Evaluates the given command (*)
  -h, --help                   Prints this message (standalone)
  -r "FILE"                    Requires the given files/patterns (*)
  -S SCRIPT                    Finds and executes the given script in \$PATH
  -pr "FILE"                   Requires the given files/patterns in parallel (*)
  -pa "PATH"                   Prepends the given path to Erlang code path (*)
  -pz "PATH"                   Appends the given path to Erlang code path (*)
  -v, --version                Prints Erlang/OTP and Elixir versions (standalone)

  --erl "SWITCHES"             Switches to be passed down to Erlang (*)
  --eval "COMMAND"             Evaluates the given command, same as -e (*)
  --logger-otp-reports BOOL    Enables or disables OTP reporting
  --logger-sasl-reports BOOL   Enables or disables SASL reporting
  --no-halt                    Does not halt the Erlang VM after execution
  --short-version              Prints Elixir version (standalone)
  --werl                       Uses Erlang's Windows shell GUI (Windows only)

Options given after the .exs file or -- are passed down to the executed code.
Options can be passed to the Erlang runtime using \$ELIXIR_ERL_OPTIONS or --erl.

## Distribution options

The following options are related to node distribution.

  --cookie COOKIE              Sets a cookie for this distributed node
  --hidden                     Makes a hidden node
  --name NAME                  Makes and assigns a name to the distributed node
  --rpc-eval NODE "COMMAND"    Evaluates the given command on the given remote node (*)
  --sname NAME                 Makes and assigns a short name to the distributed node

--name and --sname may be set to undefined so one is automatically generated.

## Release options

The following options are generally used under releases.

  --boot "FILE"                Uses the given FILE.boot to start the system
  --boot-var VAR "VALUE"       Makes \$VAR available as VALUE to FILE.boot (*)
  --erl-config "FILE"          Loads configuration in FILE.config written in Erlang (*)
  --pipe-to "PIPEDIR" "LOGDIR" Starts the Erlang VM as a named PIPEDIR and LOGDIR
  --vm-args "FILE"             Passes the contents in file as arguments to the VM

--pipe-to starts Elixir detached from console (Unix-like only).
It will attempt to create PIPEDIR and LOGDIR if they don't exist.
See run_erl to learn more. To reattach, run: to_erl PIPEDIR.

--pipe-to is not supported on Windows. If set, Elixir won't boot.

** Options marked with (*) can be given more than once.
** Standalone options can't be combined with other options.
"@

  Write-Host $Help
}

if (($Args.Count -eq 1) -and ($Args[0] -eq "--short-version")) {
  Write-Host "$ELIXIR_VERSION"
  exit
}

if (($Args.Count -eq 0) -or (($Args.Count -eq 1) -and ($Args[0] -in @("-h", "--help")))) {
  PrintElixirHelp
  exit
}

$ElixirParams = New-Object Collections.Generic.List[String]
$ErlangParams = New-Object Collections.Generic.List[String]
$BeforeExtras = New-Object Collections.Generic.List[String]
$AllOtherParams = New-Object Collections.Generic.List[String]

for ($i = 0; $i -lt $Args.Count; $i++) {
  $private:Arg = $Args[$i]

  switch ($Arg) {
    { $_ -in @("-e", "-r", "-pr", "-pa", "-pz", "--eval", "--remsh", "--dot-iex", "--dbg") } {
      $ElixirParams.Add("$Arg $($Args[++$i])")
      break
    }

    { $_ -in @("-v", "--version", "--no-halt") } {
      $ElixirParams.Add($Arg)
      break
    }

    "--cookie" {
      $ErlangParams.Add("-setcookie $($Args[++$i])")
      break
    }

    "--hidden" {
      $ErlangParams.Add("-hidden")
      break
    }

    "--name" {
      $ErlangParams.Add("-name $($Args[++$i])")
      break
    }

    "--sname" {
      $ErlangParams.Add("-sname $($Args[++$i])")
      break
    }

    "--boot" {
      $ErlangParams.Add("-boot $($Args[++$i])")
      break
    }

    "--erl-config" {
      $ErlangParams.Add("-config $($Args[++$i])")
      break
    }

    "--vm-args" {
      $ErlangParams.Add("-args_file $($Args[++$i])")
      break
    }

    "--logger-otp-reports" {
      $private:TempVal = $Args[$i + 1]
      if ($TempVal -in @("true", "false")) {

        $ErlangParams.Add("-logger handle_otp_reports $($Args[++$i])")
      }
      break
    }

    "--logger-sasl-reports" {
      $private:TempVal = $Args[$i + 1]
      if ($TempVal -in @("true", "false")) {

        $ErlangParams.Add("-logger handle_sasl $($Args[++$i])")
      }
      break
    }

    "--erl" {
      $BeforeExtras.Add($Args[++$i])
      break
    }

    "--werl" {
      if ($IsWindows) {
        $ErlExec = "werl"
      }
      break
    }

    "+iex" {
      $ElixirParams.Add("+iex")
      $UseIex = $true

      break
    }

    "+elixirc" { $ElixirParams.Add("+elixirc"); break }

    "--rpc-eval" {
      $private:Key = $Args[++$i]
      $private:Value = $Args[++$i]

      if ($null -eq $Key) {
        Write-Error "--rpc-eval: NODE must be present"
        exit
      }

      if ($null -eq $Value) {
        Write-Error "--rpc-eval: COMMAND for the '$Key' node must be present"
        exit
      }

      $ElixirParams.Add("--rpc-eval $Key $Value")
      break
    }

    "--boot-var" {
      $private:Key = $Args[++$i]
      $private:Value = $Args[++$i]

      if ($null -eq $Key) {
        Write-Error "--boot-var: VAR must be present"
        exit
      }

      if ($null -eq $Value) {
        Write-Error "--boot-var: Value for the '$Key' var must be present"
        exit
      }

      $ErlangParams.Add("-boot_var $Key $Value")
      break
    }

    "--pipe-to" {
      $private:Key = $Args[++$i]
      $private:Value = $Args[++$i]

      Write-Warning "--pipe-to: Option is not yet supported. Ignoring $Key $Value"
      break
    }

    Default {
      if ($Arg -is [string]) {
        $AllOtherParams.Add($Arg)
      }
      else {
        $AllOtherParams.Add([string]::Join(",", $Arg))
      }
      break
    }
  }
}

# Support for ANSI is only disable if TERM or NO_COLOR env vars are set.
# This will change the $PSStyle.OutputRendering property.
if ($PSStyle.OutputRendering -ne "PlainText") {
  $BeforeExtras.Insert(0, "-elixir ansi_enabled true")
}

if ($null -eq $UseIEx) {
  $BeforeExtras.Insert(0, "-s elixir start_cli")
}

$BeforeExtras.Insert(0, "-pa $(Join-Path $ScriptPath -ChildPath "../lib/elixir/ebin")")
$BeforeExtras.Insert(0, "-noshell -elixir_root $(Join-Path $ScriptPath -ChildPath "../lib")")

# One MAY change ERTS_BIN= but you MUST NOT change
# ERTS_BIN=$ERTS_BIN as it is handled by Elixir releases.
# TODO: change when we port the releases scripts.
# $ERTS_BIN=
$ERTS_BIN = "$Env:ERTS_BIN"

$ELIXIR_ERL_OPTIONS = "$Env:ELIXIR_ERL_OPTIONS"

$AllParams = New-Object Collections.Generic.List[String]

$AllParams.Add($ELIXIR_ERL_OPTIONS)
$AllParams.AddRange($ErlangParams)
$AllParams.AddRange($BeforeExtras)
$AllParams.Add("-extra")
$AllParams.AddRange($ElixirParams)
$AllParams.AddRange($AllOtherParams)

$ParamsPart = [string]::Join(" ", ($AllParams | Where-Object { $_ -ne "" }))

$BinSuffix = ""

if ($IsWindows) {
  $BinSuffix = ".exe"
}

$BinPath = "$ErlExec$BinSuffix"

if ($ERTS_BIN) {
  $BinPath = Join-Path -Path $ERTS_BIN -ChildPath $BinPath
}

$Command = "$BinPath $ParamsPart"

if ($ErlExec -eq "werl") {
  $Command = "start `"`" $Command"
}

Write-Host $Command
