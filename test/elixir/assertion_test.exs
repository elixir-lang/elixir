module AssertionTest
ExUnit::Case.prepare

def test_always_pass do
  true = true
end

def test_always_fail do
  true = false
end