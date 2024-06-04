#!/usr/bin/env pwsh

$scriptPath = Split-Path -Parent $PSCommandPath
$command = Join-Path -Path $scriptPath -ChildPath "elixir.ps1"
$mixFile = Join-Path -Path $scriptPath -ChildPath "mix"

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

$quotedArgs = $args | forEach-Object -Process { QuoteString($_) }

Invoke-Expression "$command $mixFile $($quotedArgs -join " ")"
