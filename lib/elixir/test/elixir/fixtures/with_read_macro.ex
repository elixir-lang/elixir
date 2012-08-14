defmodule WithReadMacro do
  use Kernel.ReadMacroTest.Helper
<%
  def foo(false), do: 0
%>

<%:foo:
  def foo(true),   do: 1
:foo:%>

<:ignore:
  def bar(var // 0)
  def bar(_), do: 2
:ignore:>
  def alpha() do
    body = <:html:
<html>
     <head>
          <title>
               Example
          </title>
     </head>
     <body>
          <h1>
               Hi!
          </h1>
     </body>
</html>
:html:>
    body
  end
<%:not_used:
  def nested(cond) do
    case cond do
      {true, 0} -> true
      {true, non_zero} -> case non_zero < 5 do
                            true -> :ok
                            <%:false_branch:
                            false -> :nok
                            :false_branch:%>
                          end
    end
  end
:not_used:%>
end