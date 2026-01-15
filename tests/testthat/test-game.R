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
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  stats$wordWeights <- list("alpha" = 5, "beta" = 3)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0

  # Suppress messages
  suppressMessages({
    correct <- processAnswer("A", "alpha", wordsMap, stats)
  })

  expect_true(correct)
  expect_equal(stats$totalAnswers, 1)
  expect_equal(stats$rightAnswers, 1)
  expect_equal(stats$wrongAnswers, 0)
  expect_equal(stats$wordWeights[["alpha"]], 4)
})

test_that("processAnswer updates stats correctly for wrong answer", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  stats$wordWeights <- list("alpha" = 1, "beta" = 1)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0

  suppressMessages({
    correct <- processAnswer("B", "alpha", wordsMap, stats)
  })

  expect_false(correct)
  expect_equal(stats$totalAnswers, 1)
  expect_equal(stats$rightAnswers, 0)
  expect_equal(stats$wrongAnswers, 1)
  expect_equal(stats$wordWeights[["alpha"]], 2)
})

test_that("processAnswer updates stats correctly for skipped answer", {
  wordsMap <- list("alpha" = c("A"))
  stats$wordWeights <- list("alpha" = 1)
  stats$rightAnswers <- 0
  stats$wrongAnswers <- 0
  stats$totalAnswers <- 0

  suppressMessages({
    correct <- processAnswer(character(0), "alpha", wordsMap, stats)
  })

  expect_false(correct)
  expect_equal(stats$totalAnswers, 1)
  expect_equal(stats$wrongAnswers, 1)
})

test_that("processAnswer handles words with multiple translations", {
  wordsMap <- list("lainata" = c("lend", "borrow"))
  stats$wordWeights <- list("lainata" = 5)
  stats$rightAnswers <- 0
  stats$totalAnswers <- 0

  suppressMessages({
    correct1 <- processAnswer("lend", "lainata", wordsMap, stats)
  })

  expect_true(correct1)
  expect_equal(stats$wordWeights[["lainata"]], 4)

  suppressMessages({
    correct2 <- processAnswer("borrow", "lainata", wordsMap, stats)
  })

  expect_true(correct2)
  expect_equal(stats$wordWeights[["lainata"]], 3)
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
  stats$totalAnswers <- 10
  stats$rightAnswers <- 7
  stats$wordWeights <- list("alpha" = 1)

  suppressMessages({
    continue <- handleSummaryCommand(stats)
  })

  expect_true(continue)
})

test_that("handleSummaryCommand displays stats", {
  stats$totalAnswers <- 10
  stats$rightAnswers <- 7
  stats$wordWeights <- list("alpha" = 1)

  expect_message(
    handleSummaryCommand(stats),
    "Total Answers: 10"
  )
})
