module OS
  def cmd(command)
    Erlang.os.cmd(command.to_char_list).to_bin
  end
end