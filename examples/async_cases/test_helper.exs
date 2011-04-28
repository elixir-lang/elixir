% An example of test cases running in parallel. To see it working,
% run this file as: `time exunit examples/async_cases/*_test.exs`.
%
% You will see it run all tests in about 3 seconds, instead of
% 8 seconds if all tests were run in sequence.

% Configure to run all four cases in parallel
ExUnit.configure 'max_cases: 4