test_that("new_stats creates an S3 object with correct class", {
  stats <- new_stats()

  expect_s3_class(stats, "game_stats")
})

test_that("new_stats initializes with zero counts", {
  stats <- new_stats()

  expect_equal(stats$rightAnswers, 0)
  expect_equal(stats$wrongAnswers, 0)
  expect_equal(stats$totalAnswers, 0)
})

test_that("stats has an empty wordWeights list", {
  stats <- new_stats()
  expect_true(is.list(stats$wordWeights))
  expect_length(stats$wordWeights, 0)
})

test_that("register_mistake increments wrongAnswers counter", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1)

  stats <- register_mistake(stats, "alpha")

  expect_equal(stats$wrongAnswers, 1)
})

test_that("register_mistake increments word weight", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1, "beta" = 3)

  stats <- register_mistake(stats, "alpha")

  expect_equal(stats$wordWeights[["alpha"]], 2)
})

test_that("register_mistake increments weight multiple times", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1)

  stats <- register_mistake(stats, "alpha")
  stats <- register_mistake(stats, "alpha")
  stats <- register_mistake(stats, "alpha")

  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("register_mistake doesn't affect other word weights", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1, "beta" = 5)

  stats <- register_mistake(stats, "alpha")

  expect_equal(stats$wordWeights[["beta"]], 5)
})

test_that("register_correct_answer increments rightAnswers counter", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1)

  stats <- register_correct_answer(stats, "alpha")

  expect_equal(stats$rightAnswers, 1)
})

test_that("register_correct_answer decrements word weight", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 5)

  stats <- register_correct_answer(stats, "alpha")

  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("register_correct_answer doesn't go below 1", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1)

  stats <- register_correct_answer(stats, "alpha")

  expect_equal(stats$wordWeights[["alpha"]], 1)
})

test_that("register_correct_answer can reduce high weights", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 10)

  stats <- register_correct_answer(stats, "alpha")
  stats <- register_correct_answer(stats, "alpha")
  stats <- register_correct_answer(stats, "alpha")

  expect_equal(stats$wordWeights[["alpha"]], 7)
})

test_that("register_correct_answer stays at 1 with multiple correct answers", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 2)

  stats <- register_correct_answer(stats, "alpha")
  stats <- register_correct_answer(stats, "alpha")
  stats <- register_correct_answer(stats, "alpha")

  expect_equal(stats$wordWeights[["alpha"]], 1)
})

test_that("register_correct_answer doesn't affect other word weights", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 5, "beta" = 3)

  stats <- register_correct_answer(stats, "alpha")

  expect_equal(stats$wordWeights[["beta"]], 3)
})

test_that("format.game_stats returns a string", {
  stats <- new_stats()
  stats$rightAnswers <- 5
  stats$wrongAnswers <- 3
  stats$totalAnswers <- 8
  stats$wordWeights <- list("alpha" = 1)

  output <- format(stats)

  expect_type(output, "character")
})

test_that("format.game_stats includes total answers", {
  stats <- new_stats()
  stats$totalAnswers <- 42
  stats$rightAnswers <- 30
  stats$wordWeights <- list("alpha" = 1)

  output <- format(stats)

  expect_match(output, "Total Answers: 42")
})

test_that("format.game_stats includes right answers", {
  stats <- new_stats()
  stats$totalAnswers <- 20
  stats$rightAnswers <- 15
  stats$wordWeights <- list("alpha" = 1)

  output <- format(stats)

  expect_match(output, "Right Answers: 15")
})

test_that("format.game_stats includes percentage correct", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 7
  stats$wrongAnswers <- 3
  stats$wordWeights <- list("alpha" = 1)

  output <- format(stats)

  expect_match(output, "% Correct: 70\\.0%")
})

test_that("format.game_stats handles 0 total answers", {
  stats <- new_stats()
  stats$totalAnswers <- 0
  stats$rightAnswers <- 0
  stats$wordWeights <- list("alpha" = 1)

  output <- format(stats)

  # Should show 0.0% not NaN
  expect_match(output, "% Correct: 0\\.0%")
  expect_false(grepl("NaN", output))
})

test_that("format.game_stats includes corpus size", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 5
  stats$wordWeights <- list("alpha" = 1, "beta" = 2, "gamma" = 3)

  output <- format(stats)

  expect_match(output, "Corpus size: 3")
})

test_that("format.game_stats includes top struggling words when weights > 1", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 5
  stats$wordWeights <- list(
    "alpha" = 5,
    "beta" = 3,
    "gamma" = 1
  )

  output <- format(stats)

  expect_match(output, "alpha")
  expect_match(output, "beta")
})

test_that("format.game_stats doesn't include struggling words when all weights = 1", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 10
  stats$wordWeights <- list("alpha" = 1, "beta" = 1)

  output <- format(stats)

  expect_false(grepl("struggling", output))
})

test_that("format.game_stats displays mistake count (weight - 1) not raw weight", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 5
  stats$wordWeights <- list(
    "alpha" = 4, # 3 mistakes
    "beta" = 2, # 1 mistake
    "gamma" = 1 # 0 mistakes (should not appear)
  )

  output <- format(stats)

  # Should show "alpha (3)" not "alpha (4)"
  expect_match(output, "alpha \\(3\\)")
  # Should show "beta (1)" not "beta (2)"
  expect_match(output, "beta \\(1\\)")
  # Should not include gamma since its weight is 1 (0 mistakes)
  expect_false(grepl("gamma", output))
})

test_that("weight increases then decreases correctly", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1)

  # Make mistakes
  stats <- register_mistake(stats, "alpha")
  stats <- register_mistake(stats, "alpha")
  expect_equal(stats$wordWeights[["alpha"]], 3)
  expect_equal(stats$wrongAnswers, 2)

  # Get correct answers
  stats <- register_correct_answer(stats, "alpha")
  expect_equal(stats$wordWeights[["alpha"]], 2)
  expect_equal(stats$rightAnswers, 1)

  stats <- register_correct_answer(stats, "alpha")
  expect_equal(stats$wordWeights[["alpha"]], 1)
  expect_equal(stats$rightAnswers, 2)
})

test_that("tracks multiple words independently", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1, "beta" = 1, "gamma" = 1)

  stats <- register_mistake(stats, "alpha")
  stats <- register_mistake(stats, "alpha")
  stats <- register_mistake(stats, "beta")
  stats <- register_correct_answer(stats, "gamma")

  expect_equal(stats$wordWeights[["alpha"]], 3)
  expect_equal(stats$wordWeights[["beta"]], 2)
  expect_equal(stats$wordWeights[["gamma"]], 1)
  expect_equal(stats$wrongAnswers, 3)
  expect_equal(stats$rightAnswers, 1)
})

test_that("record_answer increments totalAnswers", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 5)

  stats <- record_answer(stats, "alpha", TRUE)
  expect_equal(stats$totalAnswers, 1)

  stats <- record_answer(stats, "alpha", FALSE)
  expect_equal(stats$totalAnswers, 2)
})

test_that("record_answer calls register_correct_answer for correct answers", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 5)

  stats <- record_answer(stats, "alpha", TRUE)

  expect_equal(stats$rightAnswers, 1)
  expect_equal(stats$wrongAnswers, 0)
  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("record_answer calls register_mistake for incorrect answers", {
  stats <- new_stats()
  stats$wordWeights <- list("alpha" = 1)

  stats <- record_answer(stats, "alpha", FALSE)

  expect_equal(stats$rightAnswers, 0)
  expect_equal(stats$wrongAnswers, 1)
  expect_equal(stats$wordWeights[["alpha"]], 2)
})

test_that("print.game_stats doesn't error", {
  stats <- new_stats()
  stats$totalAnswers <- 5
  stats$rightAnswers <- 3
  stats$wordWeights <- list("alpha" = 1)

  expect_output(print(stats), "Total Answers")
})

test_that("stats object is a list underneath", {
  stats <- new_stats()

  expect_type(stats, "list")
  expect_true("rightAnswers" %in% names(stats))
  expect_true("wrongAnswers" %in% names(stats))
  expect_true("totalAnswers" %in% names(stats))
  expect_true("wordWeights" %in% names(stats))
})
