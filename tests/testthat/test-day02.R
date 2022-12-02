test_that("Game works", {
  expect_equal(f02_game_score("A Y"), 8)
  expect_equal(f02_game_score("B X"), 1)
  expect_equal(f02_game_score("C Z"), 6)
})

test_that("Game decoded works", {
  expect_equal(f02_game_decode("A Y"), "A X")
  expect_equal(f02_game_score(f02_game_decode("A Y")), 4)
  expect_equal(f02_game_decode("B X"), "B X")
  expect_equal(f02_game_score(f02_game_decode("B X")), 1)
  expect_equal(f02_game_decode("C Z"), "C X")
  expect_equal(f02_game_score(f02_game_decode("C Z")), 7)
  expect_equal(f02b(example_data_02()), 12)
})
