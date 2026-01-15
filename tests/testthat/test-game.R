test_that("checkAnswer recognizes correct answers", {
  correctAnswers <- c("one", "uno")

  result <- checkAnswer("one", correctAnswers)

  expect_true(result$correct)
  expect_false(result$skipped)
})

test_that("checkAnswer recognizes incorrect answers", {
  correctAnswers <- c("one", "uno")

  result <- checkAnswer("two", correctAnswers)

  expect_false(result$correct)
  expect_false(result$skipped)
})

test_that("checkAnswer recognizes skipped answers", {
  correctAnswers <- c("one", "uno")

  result <- checkAnswer(character(0), correctAnswers)

  expect_false(result$correct)
  expect_true(result$skipped)
})

test_that("checkAnswer handles multiple correct answers", {
  correctAnswers <- c("lend", "borrow")

  result1 <- checkAnswer("lend", correctAnswers)
  result2 <- checkAnswer("borrow", correctAnswers)

  expect_true(result1$correct)
  expect_true(result2$correct)
})

test_that("checkAnswer is case-sensitive", {
  correctAnswers <- c("one")

  result <- checkAnswer("One", correctAnswers)

  expect_false(result$correct)
})

test_that("processAnswer updates stats correctly for correct answer", {
  stats <- new_stats()
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  stats$wordWeights <- list("alpha" = 5, "beta" = 3)

  suppressMessages({
    result <- processAnswer("A", "alpha", wordsMap, stats)
  })

  expect_true(result$correct)
  expect_equal(result$stats$totalAnswers, 1)
  expect_equal(result$stats$rightAnswers, 1)
  expect_equal(result$stats$wrongAnswers, 0)
  expect_equal(result$stats$wordWeights[["alpha"]], 4)
})

test_that("processAnswer updates stats correctly for wrong answer", {
  stats <- new_stats()
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  stats$wordWeights <- list("alpha" = 1, "beta" = 1)

  suppressMessages({
    result <- processAnswer("B", "alpha", wordsMap, stats)
  })

  expect_false(result$correct)
  expect_equal(result$stats$totalAnswers, 1)
  expect_equal(result$stats$rightAnswers, 0)
  expect_equal(result$stats$wrongAnswers, 1)
  expect_equal(result$stats$wordWeights[["alpha"]], 2)
})

test_that("processAnswer updates stats correctly for skipped answer", {
  stats <- new_stats()
  wordsMap <- list("alpha" = c("A"))
  stats$wordWeights <- list("alpha" = 1)

  suppressMessages({
    result <- processAnswer(character(0), "alpha", wordsMap, stats)
  })

  expect_false(result$correct)
  expect_equal(result$stats$totalAnswers, 1)
  expect_equal(result$stats$wrongAnswers, 1)
})

test_that("processAnswer handles words with multiple translations", {
  stats <- new_stats()
  wordsMap <- list("lainata" = c("lend", "borrow"))
  stats$wordWeights <- list("lainata" = 5)

  suppressMessages({
    result1 <- processAnswer("lend", "lainata", wordsMap, stats)
  })

  expect_true(result1$correct)
  expect_equal(result1$stats$wordWeights[["lainata"]], 4)

  suppressMessages({
    result2 <- processAnswer("borrow", "lainata", wordsMap, result1$stats)
  })

  expect_true(result2$correct)
  expect_equal(result2$stats$wordWeights[["lainata"]], 3)
})

test_that("provideFeedback generates correct message", {
  # Correct answer
  result <- list(correct = TRUE, skipped = FALSE)
  expect_message(
    provideFeedback(result, "one", c("one")),
    "Correct!"
  )

  # Wrong answer
  result <- list(correct = FALSE, skipped = FALSE)
  expect_message(
    provideFeedback(result, "two", c("one")),
    "Wrong!"
  )

  # Skipped
  result <- list(correct = FALSE, skipped = TRUE)
  expect_message(
    provideFeedback(result, character(0), c("one")),
    "Skipping!"
  )
})

test_that("provideFeedback shows all correct answers", {
  result <- list(correct = FALSE, skipped = FALSE)

  msg <- capture_messages({
    provideFeedback(result, "wrong", c("lend", "borrow"))
  })

  expect_true(any(grepl("LEND or BORROW", msg)))
})

test_that("promptForTranslation formats message correctly", {
  msg <- capture_messages({
    promptForTranslation("yksi", 25.5)
  })

  expect_true(any(grepl("YKSI", msg)))
  expect_true(any(grepl("25.5%", msg)))
})

test_that("handleSummaryCommand returns TRUE to continue game", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 7
  stats$wordWeights <- list("alpha" = 1)

  suppressMessages({
    continue <- handleSummaryCommand(stats)
  })

  expect_true(continue)
})

test_that("handleSummaryCommand displays stats", {
  stats <- new_stats()
  stats$totalAnswers <- 10
  stats$rightAnswers <- 7
  stats$wordWeights <- list("alpha" = 1)

  expect_message(
    handleSummaryCommand(stats),
    "Total Answers: 10"
  )
})
