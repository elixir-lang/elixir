#!/usr/bin/env pwsh

$scriptName = Split-Path -Leaf $PSCommandPath

if ($args[0] -in @("-h", "--help")) {
  Write-Host @"
Usage: $scriptName [options] [.exs file] [data]

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

$scriptPath = Split-Path -Parent $PSCommandPath
$command = Join-Path -Path $scriptPath -ChildPath "elixir.ps1"
$quotedArgs = $args | forEach-Object -Process { QuoteString($_) }

Invoke-Expression "$command --no-halt --erl `"-user elixir`" +iex $($quotedArgs -join " ")"
