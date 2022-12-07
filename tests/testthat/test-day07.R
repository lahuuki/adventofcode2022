test_that("Example Works", {
  expect_equal(f07a(example_data_07()), 95437)
  expect_equal(as.integer(f07b(example_data_07())), 24933642)
})
