#!/usr/bin/env pwsh

$ELIXIR_VERSION = "1.18.0-dev"

$scriptPath = Split-Path -Parent $PSCommandPath
$erlExec = "erl"

function PrintElixirHelp {
  $scriptName = Split-Path -Leaf $PSCommandPath
  $help = @"
Usage: $scriptName [options] [.exs file] [data]

## General options

  -e "COMMAND"                 Evaluates the given command (*)
  -h, --help                   Prints this message (standalone)
  -r "FILE"                    Requires the given files/patterns (*)
  -S SCRIPT                    Finds and executes the given script in `$PATH
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

Options given after the .exs file or -- are passed down to the executed code.
Options can be passed to the Erlang runtime using `$ELIXIR_ERL_OPTIONS or --erl.

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
  --boot-var VAR "VALUE"       Makes `$VAR available as VALUE to FILE.boot (*)
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

  Write-Host $help
}

if (($args.Count -eq 1) -and ($args[0] -eq "--short-version")) {
  Write-Host "$ELIXIR_VERSION"
  exit
}

if (($args.Count -eq 0) -or (($args.Count -eq 1) -and ($args[0] -in @("-h", "--help")))) {
  PrintElixirHelp
  exit 1
}

function NormalizeArg {
  param(
    [Parameter(Mandatory = $true, ValueFromPipeline = $true)]
    [string[]] $Items
  )
  $Items -join ","
}

function QuoteString {
  param(
    [Parameter(ValueFromPipeline = $true)]
    [string] $Item
  )

  # We surround the string with double quotes, in order to preserve its contents as
  # only one command arg.
  # This is needed because PowerShell consider spaces as separator of arguments.
  # The double quotes around will be removed when PowerShell process the argument.
  # See: https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.4#passing-quoted-strings-to-external-commands
  if ($Item.Contains(" ")) {
    '"' + $Item + '"'
  }
  else {
    $Item
  }
}

$elixirParams = @()
$erlangParams = @()
$beforeExtras = @()
$allOtherParams = @()

$runErlPipe = $null
$runErlLog = $null

for ($i = 0; $i -lt $args.Count; $i++) {
  $private:arg = $args[$i]

  switch ($arg) {
    { $_ -in @("-e", "-r", "-pr", "-pa", "-pz", "--eval", "--remsh", "--dot-iex", "--dbg") } {
      $private:nextArg = NormalizeArg($args[++$i])

      $elixirParams += $arg
      $elixirParams += $nextArg

      break
    }

    { $_ -in @("-v", "--version") } {
      # Standalone options goes only once in the Elixir params, when they are empty.
      if (($elixirParams.Count -eq 0) -and ($allOtherParams.Count -eq 0)) {
        $elixirParams += $arg
      }
      else {
        $allOtherParams += $arg
      }
      break
    }

    "--no-halt" {
      $elixirParams += $arg
      break
    }

    "--cookie" {
      $erlangParams += "-setcookie"
      $erlangParams += $args[++$i]
      break
    }

    "--hidden" {
      $erlangParams += "-hidden"
      break
    }

    "--name" {
      $erlangParams += "-name"
      $erlangParams += $args[++$i]
      break
    }

    "--sname" {
      $erlangParams += "-sname"
      $erlangParams += $args[++$i]
      break
    }

    "--boot" {
      $erlangParams += "-boot"
      $erlangParams += $args[++$i]
      break
    }

    "--erl-config" {
      $erlangParams += "-config"
      $erlangParams += $args[++$i]
      break
    }

    "--vm-args" {
      $erlangParams += "-args_file"
      $erlangParams += $args[++$i]
      break
    }

    "--logger-otp-reports" {
      $private:tempVal = $args[$i + 1]
      if ($tempVal -in @("true", "false")) {
        $erlangParams.AddRange([string[]]@("-logger", "handle_otp_reports", $args[++$i]))
      }
      break
    }

    "--logger-sasl-reports" {
      $private:tempVal = $args[$i + 1]
      if ($tempVal -in @("true", "false")) {
        $erlangParams.AddRange([string[]]@("-logger", "handle_sasl_reports", $args[++$i]))
      }
      break
    }

    "--erl" {
      $private:erlFlags = $args[++$i] -split " "
      $beforeExtras += $erlFlags
      break
    }

    "+iex" {
      $elixirParams += "+iex"
      $useIex = $true

      break
    }

    "+elixirc" {
      $elixirParams += "+elixirc"
      break
    }

    "--rpc-eval" {
      $private:key = $args[++$i]
      $private:value = $args[++$i]

      if ($null -eq $key) {
        Write-Error "--rpc-eval: NODE must be present"
        exit 1
      }

      if ($null -eq $value) {
        Write-Error "--rpc-eval: COMMAND for the '$key' node must be present"
        exit 1
      }

      $elixirParams += "--rpc-eval"
      $elixirParams += $key
      $elixirParams += $value
      break
    }

    "--boot-var" {
      $private:key = $args[++$i]
      $private:value = $args[++$i]

      if ($null -eq $key) {
        Write-Error "--boot-var: VAR must be present"
        exit 1
      }

      if ($null -eq $value) {
        Write-Error "--boot-var: Value for the '$key' var must be present"
        exit 1
      }

      $elixirParams += "-boot_var"
      $elixirParams += $key
      $elixirParams += $value
      break
    }

    "--pipe-to" {
      $runErlPipe = $args[++$i]
      $runErlLog = $args[++$i]

      if ($null -eq $runErlPipe) {
        Write-Error "--pipe-to: PIPEDIR must be present"
        exit 1
      }

      if ($null -eq $runErlLog) {
        Write-Error "--pipe-to: PIPELOG must be present"
        exit 1
      }

      if ($runErlPipe.EndsWith("/") -or $runErlLog.EndsWith("/")) {
        Write-Error "--pipe-to: PIPEDIR and PIPELOG must not end with a slash"
        exit 1
      }

      break
    }

    Default {
      $private:normalized = NormalizeArg $arg
      $allOtherParams += $normalized
      break
    }
  }
}

if ($null -eq $useIEx) {
  $beforeExtras = @("-s", "elixir", "start_cli") + $beforeExtras
}

$beforeExtras = @("-pa", "$(Join-Path $scriptPath -ChildPath "../lib/elixir/ebin")") + $beforeExtras
$beforeExtras = @("-noshell", "-elixir_root", "$(Join-Path $scriptPath -ChildPath "../lib")") + $beforeExtras

# One MAY change ERTS_BIN= but you MUST NOT change
# ERTS_BIN=$ERTS_BIN as it is handled by Elixir releases.
# TODO: change when we port the releases scripts.
# $ERTS_BIN=
$ERTS_BIN = "$env:ERTS_BIN"

$allParams = @()

if ($null -ne $env:ELIXIR_ERL_OPTIONS) {
  $private:erlFlags = $env:ELIXIR_ERL_OPTIONS -split " "
  $allParams += $erlFlags
}

$allParams += $erlangParams
$allParams += $beforeExtras
$allParams += "-extra"
$allParams += $elixirParams
$allParams += $allOtherParams

$binSuffix = ""

# The variable is available after PowerShell 7.2. Previous to that, PS only worked on Windows.
if ($isWindows -or ($null -eq $isWindows)) {
  $binSuffix = ".exe"
}

$binPath = "$erlExec$binSuffix"

if ($ERTS_BIN) {
  $binPath = Join-Path -Path $ERTS_BIN -ChildPath $binPath
}

if ($null -eq $runErlPipe) {
  # We double the double-quotes because they are going to be escaped by arguments parsing.
  $paramsPart = $allParams | ForEach-Object -Process { QuoteString($_ -replace "`"", "`"`"") }
}
else {
  $allParams.Insert(0, $binPath)

  $erlExec = "run_erl"
  $binPath = "$erlExec$binSuffix"

  if ($ERTS_BIN) {
    $binPath = Join-Path -Path $ERTS_BIN -ChildPath $binPath
  }

  # We scape special chars using the Unix style of scaping, with "\".
  # But first we escape the double-quotes, because for some reason they are not escaped in the same regex.
  $private:escaped = $allParams | ForEach-Object -Process { ($_ -replace "`"", "\`"") -replace "[^a-zA-Z0-9_/-]", "\$&" }

  # The args are surrounded here because we want to have only one argument for the entire command.
  $paramsPart = @("-daemon","`"$runErlPipe/`"", "`"$runErlLog/`"", "`"$($escaped -join " ")`"")
}

if ($env:ELIXIR_CLI_DRY_RUN) {
  Write-Host "$binPath $paramsPart"
}
else {
  if ($runErlPipe) {
    $null = New-Item -Path "." -ItemType "directory" -Name "$runErlPipe" -Force
    $null = New-Item -Path "." -ItemType "directory" -Name "$runErlLog" -Force
  }
  $output = Start-Process -FilePath $binPath -ArgumentList $paramsPart -NoNewWindow -Wait -PassThru
  exit $output.ExitCode
}
