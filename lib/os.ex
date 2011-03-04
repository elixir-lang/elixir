module OS
  def cmd(command)
    String.new Erlang.os.cmd(command.to_char_list)
  end
end