#!/usr/bin/env pwsh

$scriptPath = Split-Path -Parent $PSCommandPath
$elixirMainScript = Join-Path -Path $scriptPath -ChildPath "elixir.ps1"

$mixFile = Join-Path -Path $scriptPath -ChildPath "mix"

$prependedArgs = @($mixFile)

$allArgs = $prependedArgs + $args

# The dot is going to evaluate the script with the vars defined here.
. $elixirMainScript
