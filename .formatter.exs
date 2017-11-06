[
  inputs: [
    "lib/*/{lib,unicode,test}/**/*.{ex,exs}",
    "lib/*/mix.exs"
  ],

  locals_without_parens: [
    # Formatter tests
    assert_format: 2,
    assert_format: 3,
    assert_same: 1,
    assert_same: 2,

    # Errors tests
    assert_eval_raise: 3,

    # Mix tests
    in_fixture: 2,
    in_tmp: 2
  ]
]
