test_that("Examples work", {
  expect_equal(f05a(example_data_05()), "CMZ")
  expect_equal(f05b(example_data_05()), "MCD")
})
