@echo off
echo There is still a problem with exclamation marks!
echo The following 2 lines should be identical, except for the exclamation mark
call elixir -e "IO.puts \"zz #{IO.ANSI.green}Ahh ! Finally #{IO.ANSI.red}Colors work?""
call elixir -e "IO.puts \"zz #{IO.ANSI.green}Ahh ? Finally #{IO.ANSI.red}Colors work?""