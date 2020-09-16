[
  inputs: [
    "lib/*/{lib,unicode,test}/**/*.ex",
    "lib/**/*.exs",
    ".formatter.exs"
  ],
  locals_without_parens: [
    # Formatter tests
    assert_format: 2,
    assert_format: 3,
    assert_same: 1,
    assert_same: 2,

    # Errors tests
    assert_eval_raise: 3
  ]
]
