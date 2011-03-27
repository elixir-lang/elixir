Code.require File.expand_path("../test_helper", __FILE__)

object TupleTest
  proto ExUnit::Case

  def brackets_test
    1 = {1,2,3}[0]
    2 = {1,2,3}[1]
    3 = {1,2,3}[2]
    self.assert_error 'badarg, do
      {1,2,3}[3]
    end
  end

  def brackets_negative_index_test
    1 = {1,2,3}[-3]
    2 = {1,2,3}[-2]
    3 = {1,2,3}[-1]
    self.assert_error 'badarg, do
      {1,2,3}[-4]
    end
  end

  def set_test
    { 4, 2, 3 } = { 1, 2, 3 }.set(0, 4)
    { 1, 2, 4 } = { 1, 2, 3 }.set(2, 4)
    { 4, 2, 3 } = { 1, 2, 3 }.set(-3, 4)
    { 1, 2, 4 } = { 1, 2, 3 }.set(-1, 4)
  end

  def unpack_test
    { x, x } = { 1, 1 }
    1 = x

    assert_error { 'badmatch, { 1, 2 } }, do
      { y, y } = { 1, 2 }
    end
  end

  def empty_test
    false = {"abc"}.empty?
    true  = {}.empty?
  end

  def to_list_test
    [1,2,3] = {1,2,3}.to_list
  end

  def length_size_test
    3 = {1,2,3}.length
    0 = {}.size
  end
end