#!/usr/bin/env pwsh

$scriptPath = Split-Path -Parent $PSCommandPath
$command = Join-Path -Path $scriptPath -ChildPath "elixir.ps1"
$mixFile = Join-Path -Path $scriptPath -ChildPath "mix"

Invoke-Expression "$command $mixFile $($args -join " ")"
