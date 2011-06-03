Code.require_file "../test_helper", __FILE__

module GenTcpTest
  mixin ExUnit::Case

  module Server
    def start(pid)
      {'ok, tcp} = GenTCP.listen(5678, ['binary, {'packet, 0}, {'active, false}])
      {'ok, sock} = tcp.accept
      {'ok, bin} = recv(tcp, sock, [])
      'ok = tcp.close
      pid <- { Process.current, bin }
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

  def failed_connection_test
    % What are the odds of someone running something on this door?
    {'error, 'econnrefused} = GenTCP.connect("localhost", 6969, [{'active,false},{'packet,2}])
  end

  def server_and_client_test
    % Spawn the server in a new process
    client = Process.current
    server = Process.spawn -> GenTcpTest::Server.start(client)

    % Connect to the server
    {'ok, tcp} = GenTCP.connect("localhost", 5678, ['binary, {'packet, 0}])
    'ok = tcp.send("Some Data")
    'ok = tcp.close

    % Check if the server replied us back
    receive { ~server, "Some Data" }
      % Ok, test passed
    after 1000
      self.error "Expected to receive \"Some Data\" from server, but did not"
    end
  end
end
