# Tests for stats.R functions

test_that("stats can be reset to initial state", {
  # Reset stats to initial state
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0
  
  expect_equal(stats$rightAnswers, 0)
  expect_equal(stats$wrongAnswers, 0)
  expect_equal(stats$totalAnswers, 0)
})

test_that("stats has an empty wordWeights list", {
  expect_true(is.list(stats$wordWeights))
})

test_that("stats has required functions", {
  expect_true(is.function(stats$formatStats))
  expect_true(is.function(stats$registerMistake))
  expect_true(is.function(stats$registerCorrectAnswer))
})

test_that("registerMistake increments wrongAnswers counter", {
  # Reset stats
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$wordWeights <- list("alpha" = 1)
  
  stats$registerMistake("alpha")
  
  expect_equal(stats$wrongAnswers, 1)
})

test_that("registerMistake increments word weight", {
  stats$wordWeights <- list("alpha" = 1, "beta" = 3)
  
  stats$registerMistake("alpha")
  
  expect_equal(stats$wordWeights[["alpha"]], 2)
})

test_that("registerMistake increments weight multiple times", {
  stats$wordWeights <- list("alpha" = 1)
  
  stats$registerMistake("alpha")
  stats$registerMistake("alpha")
  stats$registerMistake("alpha")
  
  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("registerMistake doesn't affect other word weights", {
  stats$wordWeights <- list("alpha" = 1, "beta" = 5)
  
  stats$registerMistake("alpha")
  
  expect_equal(stats$wordWeights[["beta"]], 5)
})

test_that("registerCorrectAnswer increments rightAnswers counter", {
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$wordWeights <- list("alpha" = 1)
  
  stats$registerCorrectAnswer("alpha")
  
  expect_equal(stats$rightAnswers, 1)
})

test_that("registerCorrectAnswer decrements word weight", {
  stats$wordWeights <- list("alpha" = 5)
  
  stats$registerCorrectAnswer("alpha")
  
  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("registerCorrectAnswer doesn't go below 1", {
  stats$wordWeights <- list("alpha" = 1)
  
  stats$registerCorrectAnswer("alpha")
  
  expect_equal(stats$wordWeights[["alpha"]], 1)
})

test_that("registerCorrectAnswer can reduce high weights", {
  stats$wordWeights <- list("alpha" = 10)
  
  stats$registerCorrectAnswer("alpha")
  stats$registerCorrectAnswer("alpha")
  stats$registerCorrectAnswer("alpha")
  
  expect_equal(stats$wordWeights[["alpha"]], 7)
})

test_that("registerCorrectAnswer stays at 1 with multiple correct answers", {
  stats$wordWeights <- list("alpha" = 2)
  
  stats$registerCorrectAnswer("alpha")
  stats$registerCorrectAnswer("alpha")
  stats$registerCorrectAnswer("alpha")
  
  expect_equal(stats$wordWeights[["alpha"]], 1)
})

test_that("registerCorrectAnswer doesn't affect other word weights", {
  stats$wordWeights <- list("alpha" = 5, "beta" = 3)
  
  stats$registerCorrectAnswer("alpha")
  
  expect_equal(stats$wordWeights[["beta"]], 3)
})

test_that("formatStats returns a string", {
  stats$rightAnswers <- 5
  stats$wrongAnswers <- 3
  stats$totalAnswers <- 8
  stats$wordWeights <- list("alpha" = 1)
  
  output <- stats$formatStats()
  
  expect_type(output, "character")
})

test_that("formatStats includes total answers", {
  stats$totalAnswers <- 42
  stats$rightAnswers <- 30
  stats$wordWeights <- list("alpha" = 1)
  
  output <- stats$formatStats()
  
  expect_match(output, "42")
})

test_that("formatStats includes right answers", {
  stats$totalAnswers <- 20
  stats$rightAnswers <- 15
  stats$wordWeights <- list("alpha" = 1)
  
  output <- stats$formatStats()
  
  expect_match(output, "15")
})

test_that("formatStats includes percentage correct", {
  stats$totalAnswers <- 10
  stats$rightAnswers <- 7
  stats$wrongAnswers <- 3
  stats$wordWeights <- list("alpha" = 1)
  
  output <- stats$formatStats()
  
  expect_match(output, "70")
})

test_that("formatStats handles 0 total answers", {
  stats$totalAnswers <- 0
  stats$rightAnswers <- 0
  stats$wordWeights <- list("alpha" = 1)
  
  output <- stats$formatStats()
  
  # Should show 0% not NaN
  expect_match(output, "0")
  expect_false(grepl("NaN", output))
})

test_that("formatStats includes corpus size", {
  stats$totalAnswers <- 10
  stats$rightAnswers <- 5
  stats$wordWeights <- list("alpha" = 1, "beta" = 2, "gamma" = 3)
  
  output <- stats$formatStats()
  
  expect_match(output, "3")
})

test_that("formatStats includes top struggling words when weights > 1", {
  stats$totalAnswers <- 10
  stats$rightAnswers <- 5
  stats$wordWeights <- list(
    "alpha" = 5,
    "beta" = 3,
    "gamma" = 1
  )
  
  output <- stats$formatStats()
  
  expect_match(output, "alpha")
  expect_match(output, "beta")
})

test_that("formatStats doesn't include struggling words when all weights = 1", {
  stats$totalAnswers <- 10
  stats$rightAnswers <- 10
  stats$wordWeights <- list("alpha" = 1, "beta" = 1)
  
  output <- stats$formatStats()
  
  expect_false(grepl("struggling", output))
})

test_that("weight increases then decreases correctly", {
  stats$wordWeights <- list("alpha" = 1)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  
  # Make mistakes
  stats$registerMistake("alpha")
  stats$registerMistake("alpha")
  expect_equal(stats$wordWeights[["alpha"]], 3)
  expect_equal(stats$wrongAnswers, 2)
  
  # Get correct answers
  stats$registerCorrectAnswer("alpha")
  expect_equal(stats$wordWeights[["alpha"]], 2)
  expect_equal(stats$rightAnswers, 1)
  
  stats$registerCorrectAnswer("alpha")
  expect_equal(stats$wordWeights[["alpha"]], 1)
  expect_equal(stats$rightAnswers, 2)
})

test_that("tracks multiple words independently", {
  stats$wordWeights <- list("alpha" = 1, "beta" = 1, "gamma" = 1)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  
  stats$registerMistake("alpha")
  stats$registerMistake("alpha")
  stats$registerMistake("beta")
  stats$registerCorrectAnswer("gamma")
  
  expect_equal(stats$wordWeights[["alpha"]], 3)
  expect_equal(stats$wordWeights[["beta"]], 2)
  expect_equal(stats$wordWeights[["gamma"]], 1)
  expect_equal(stats$wrongAnswers, 3)
  expect_equal(stats$rightAnswers, 1)
})

test_that("recordAnswer increments totalAnswers", {
  stats$wordWeights <- list("alpha" = 5)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0
  
  stats$recordAnswer("alpha", TRUE)
  expect_equal(stats$totalAnswers, 1)
  
  stats$recordAnswer("alpha", FALSE)
  expect_equal(stats$totalAnswers, 2)
})

test_that("recordAnswer calls registerCorrectAnswer for correct answers", {
  stats$wordWeights <- list("alpha" = 5)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0
  
  stats$recordAnswer("alpha", TRUE)
  
  expect_equal(stats$rightAnswers, 1)
  expect_equal(stats$wrongAnswers, 0)
  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("recordAnswer calls registerMistake for incorrect answers", {
  stats$wordWeights <- list("alpha" = 1)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0
  
  stats$recordAnswer("alpha", FALSE)
  
  expect_equal(stats$rightAnswers, 0)
  expect_equal(stats$wrongAnswers, 1)
  expect_equal(stats$wordWeights[["alpha"]], 2)
})
