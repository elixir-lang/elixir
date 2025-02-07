# SPDX-License-Identifier: Apache-2.0
# SPDX-FileCopyrightText: 2021 The Elixir Team
# SPDX-FileCopyrightText: 2012 Plataformatec

# Store path to mix.bat as a FileInfo object
$mixBatPath = (Get-ChildItem (((Get-ChildItem $MyInvocation.MyCommand.Path).Directory.FullName) + '\mix.bat'))
$newArgs = @()

for ($i = 0; $i -lt $args.length; $i++)
{
  if ($args[$i] -is [array])
  {
    # Commas created the array so we need to reintroduce those commas
    for ($j = 0; $j -lt $args[$i].length - 1; $j++)
    {
      $newArgs += ($args[$i][$j] + ',')
    }
    $newArgs += $args[$i][-1]
  }
  else
  {
    $newArgs += $args[$i]
  }
}

# Corrected arguments are ready to pass to batch file
& $mixBatPath $newArgs