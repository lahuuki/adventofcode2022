test_that("Object find works", {
  expect_equal(f03_get_repeat_items("vJrwpWtwJgWrhcsFMMfFFhFp"), "p")
  expect_equal(f03_get_repeat_items("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"), "L")
  expect_equal(f03_get_repeat_items("CrZsJsPPZsGzwwsLwLmpwMDwp"), "s")
})

test_that("Example sums work",{
  expect_equal(f03a(example_data_03()), 157)
  expect_equal(f03b(example_data_03()), 70)
})
