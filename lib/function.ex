object Function
  def arity
    { 'arity, value } = Erlang.fun_info(self, 'arity)
    value
  end
end