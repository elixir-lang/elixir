module Chat
  module Client
    object Room
      def constructor(room)
        { 'room: room }
      end

      def join
        send 'join
      end

      def say(message)
        send { 'say, message }
      end

      def leave
        send 'leave
      end

      def flush
        room = @room
        receive { room, { 'message, message } }
          IO.puts message
        end
      end

      private
      
      def send(message)
        room = @room

        room <- { Process.self, message }

        receive { room, response }
          response
        after 1000
          IO.puts "Connection to room timed out!"
        end
      end
    end
  end

  module Server
    object Room
      def constructor(clients)
        { 'clients: clients }
      end

      def loop
        receive
        match { pid, 'join }
          notify_all Process.self, "Some user joined"
          pid <- { Process.self, 'ok }
          self.set_ivar('clients, [pid|@clients]).loop
        match { pid, { 'say, message } }
          notify_all(pid, message)
          pid <- { Process.self, 'ok }
          loop
        match { pid, 'leave }
          pid <- { Process.self, 'ok }
          room = self.set_ivar 'clients, @clients.delete(pid)
          room.notify_all Process.self, "Some user left"
          room.loop
        end
      end

      protected

      def notify_all(sender, message)
        @clients.each do (c)
          if c != sender
            c <- { Process.self, { 'message, message } }
          end
        end
      end
    end
  end
end

client = Process.self
server = Process.spawn -> Chat::Server::Room.new([client]).loop

room = Chat::Client::Room.new(server)

% Spawn another process to simulate another client that will
% join the room, send a message and leave.
Process.spawn do
  another_client = Chat::Client::Room.new(server)
  another_client.join
  another_client.say "Hi!"
  another_client.leave
end

3.times -> room.flush