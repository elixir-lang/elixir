Code.require File.expand_path("../test_helper", __FILE__)
Code.require "gen_tcp"

object GenTcpTest
  proto ExUnit::Case

  def connect_test
    {'error, 'timeout} = GenTCP.connect("google.com", 80, [{'active,false},{'packet,2}])
  end

end
