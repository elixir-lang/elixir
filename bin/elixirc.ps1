#!/usr/bin/env pwsh

$scriptName = Split-Path -Leaf $PSCommandPath

if (($args.Count -eq 0) -or ($args[0] -in @("-h", "--help"))) {
  Write-Host @"
Usage: $scriptName [elixir switches] [compiler switches] [.ex files]

  -h, --help                Prints this message and exits
  -o                        The directory to output compiled files
  -v, --version             Prints Elixir version and exits (standalone)

  --ignore-module-conflict  Does not emit warnings if a module was previously defined
  --no-debug-info           Does not attach debug info to compiled modules
  --no-docs                 Does not attach documentation to compiled modules
  --profile time            Profile the time to compile modules
  --verbose                 Prints compilation status
  --warnings-as-errors      Treats warnings as errors and returns non-zero exit status

** Options given after -- are passed down to the executed code
** Options can be passed to the Erlang runtime using ELIXIR_ERL_OPTIONS
** Options can be passed to the Erlang compiler using ERL_COMPILER_OPTIONS
"@
  exit
}

$scriptPath = Split-Path -Parent $PSCommandPath
$elixirMainScript = Join-Path -Path $scriptPath -ChildPath "elixir.ps1"

$prependedArgs = @("+elixirc")

$allArgs = $prependedArgs + $args

# The dot is going to evaluate the script with the vars defined here.
. $elixirMainScript
