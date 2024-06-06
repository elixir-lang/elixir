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

$scriptPath = Split-Path -Parent $PSCommandPath
$elixirMainScript = Join-Path -Path $scriptPath -ChildPath "elixir.ps1"

$prependedArgs = @("--no-halt", "--erl", "-user elixir", "+iex") 

$allArgs = $prependedArgs + $args

# The dot is going to evaluate the script with the vars defined here.
. $elixirMainScript
