test_that("Example 1 works", {
  expect_equal(f01a(example_data_01()), 24000)
})

test_that("Example 2 works", {
  expect_equal(f01b(example_data_01()), 45000)
})
