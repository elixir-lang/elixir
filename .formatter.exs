[
  inputs: [
    "lib/*/{lib,scripts,unicode,test}/**/*.{ex,exs}",
    "lib/*/*.exs",
    "lib/ex_unit/examples/*.exs",
    ".formatter.exs"
  ],
  locals_without_parens: [
    # Formatter tests
    assert_format: 2,
    assert_format: 3,
    assert_same: 1,
    assert_same: 2,

    # Errors tests
    assert_eval_raise: 3,

    # Float tests
    float_assert: 1
  ]
]
