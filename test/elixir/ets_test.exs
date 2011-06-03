Code.require_file "../test_helper", __FILE__

module ETSTest
  mixin ExUnit::Case

  def setup(test)
    table = ETS.create 'sometable, setup_options(test)
    table.insert {"Andrew", 21, 'boy, 'minsk}
    table.insert {"Mary", 19, 'girl, 'moscow}
    table.insert {"Jane", 20, 'girl, 'london}
    table.insert {"Jack", 21, 'boy, 'paris}
    @('table, table)
  end

  def teardown(_)
    @table.delete
  end

  % General tests for each table type

  def ets_set_test
    1     = @table.lookup("Mary").size
    true  = @table.update_element "Mary", 4, 'oslo
    false = @table.update_element "John", 2, 18
    self.assert_error 'badarg, -> @table.update_element "Mary", 1, "Suzzy"
  end

  def ets_ord_test
    21 = @table.last
    19 = @table.first
    [{"Jack", _, _, _}] = @table.lookup 21
  end

  def ets_bag_test
    [{_, year, sex, _}] = @table.lookup "Jane"
    false = @table.insert_new({"Jane", year, sex, 'rome})

    [{_, _, _, 'london}] = @table.lookup "Jane"

    ['boy] = @table.lookup_element "Andrew", 3
    true   = @table.member? "Andrew"
    true   = @table.include? "Andrew"

    self.assert_error 'badarg, -> @table.lookup_element "Peter", 3
    self.assert_error 'badarg, -> @table.update_element "Mary", 2, 21

    @table.insert({"Jane", 20, 'girl, 'london})
    @table.insert({"Jane", 20, 'girl, 'berlin})

    [20, 20] = @table.lookup_element "Jane", 2
    2 = @table.lookup("Jane").size
  end

  def ets_dup_test
    @table.insert({"Jane", 20, 'girl, 'london})
    @table.insert({"Jane", 22, 'girl, 'berlin})
    3 = @table.lookup("Jane").size
  end

  % Specific tests for each method

  private

  def setup_options 'ets_ord_test
    ['ordered_set, 'keypos/2]
  end

  def setup_options 'ets_bag_test
    ['bag]
  end

  def setup_options 'ets_dup_test
    ['duplicate_bag]
  end

  def setup_options _
    ['set]
  end
end