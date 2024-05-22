#!/usr/bin/env pwsh

$ScriptName = Split-Path -Leaf $PSCommandPath

if ($Args[0] -in @("-h", "--help")) {
  Write-Host @"
Usage: $ScriptName [options] [.exs file] [data]

The following options are exclusive to IEx:

  --dbg pry           Sets the backend for Kernel.dbg/2 to IEx.pry/0
  --dot-iex "FILE"    Evaluates FILE, line by line, to set up IEx' environment.
                      Defaults to evaluating .iex.exs or ~/.iex.exs, if any exists.
                      If FILE is empty, then no file will be loaded.
  --remsh NAME        Connects to a node using a remote shell.

It accepts all other options listed by "elixir --help".
"@
  exit
}

$ScriptPath = Split-Path -Parent $PSCommandPath
$Command = Join-Path -Path $ScriptPath -ChildPath "elixir.ps1"

$NewArgs = @("--no-halt", "--erl `"-user elixir`"", "+iex")

for ($i = 0; $i -lt $Args.Count; $i++) {
  $Arg = $Args[$i]

  if ($Arg -is [string]) {
    $NewArgs += $Arg
  }
  else {
    $NewArgs += [string]::Join(",", $Arg)
  }
}

& $Command $NewArgs
