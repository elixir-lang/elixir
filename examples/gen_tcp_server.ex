Code.require "gen_tcp"

module Server
  def start
    {'ok, tcp} = GenTCP.listen(5678, ['binary, {'packet, 0}, {'active, false}])
    {'ok, sock} = tcp.accept
    {'ok, bin} = recv(tcp, sock, [])
    'ok = tcp.close
    IO.puts bin.to_s
  end

  private

  def recv(tcp, sock, bs)
    case tcp.recv(sock,0)
    match {'ok, b}
      recv(tcp, sock, [bs, b])
    match {'error, 'closed}
      {'ok, bs.to_bin}
    end
  end
end

Server.start