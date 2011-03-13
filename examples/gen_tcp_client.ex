Code.require "gen_tcp"
{'ok, tcp} = GenTCP.connect("localhost", 5678, ['binary, {'packet, 0}])
'ok = tcp.send("Some Data")
'ok = tcp.close

