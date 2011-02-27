module GenServer
  protected

  def __added_as_proto__(base)
    base.define_module_attribute('behavior, 'gen_server)
  end
end