
test_that("sampleWord returns a word from the wordsMap", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"), "gamma" = c("C"))
  wordWeights <- list("alpha" = 1, "beta" = 1, "gamma" = 1)
  
  set.seed(42)
  word <- sampleWord(wordsMap, wordWeights)
  
  expect_true(word %in% names(wordsMap))
})

test_that("sampleWord returns a single word (character vector of length 1)", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("alpha" = 1, "beta" = 1)
  
  word <- sampleWord(wordsMap, wordWeights)
  
  expect_length(word, 1)
  expect_type(word, "character")
})

test_that("sampleWord respects weights in sampling", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"), "gamma" = c("C"))
  wordWeights <- list("alpha" = 100, "beta" = 1, "gamma" = 1)
  
  set.seed(42)
  samples <- replicate(1000, sampleWord(wordsMap, wordWeights))
  
  # Alpha should be sampled much more frequently
  alpha_count <- sum(samples == "alpha")
  expect_gt(alpha_count, 900)
})

test_that("sampleWord handles equal weights uniformly", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"), "gamma" = c("C"))
  wordWeights <- list("alpha" = 1, "beta" = 1, "gamma" = 1)
  
  set.seed(42)
  samples <- replicate(3000, sampleWord(wordsMap, wordWeights))
  
  # With equal weights, distribution should be roughly equal
  counts <- table(samples)
  expect_true(all(counts > 800))
  expect_true(all(counts < 1200))
})

test_that("sampleWord works with single word", {
  wordsMap <- list("alpha" = c("A"))
  wordWeights <- list("alpha" = 1)
  
  word <- sampleWord(wordsMap, wordWeights)
  
  expect_equal(word, "alpha")
})

test_that("sampleWord handles words with multiple translations", {
  wordsMap <- list(
    "lainata" = c("lend", "borrow"),
    "nousta" = c("get up", "increase")
  )
  wordWeights <- list("lainata" = 1, "nousta" = 1)
  
  word <- sampleWord(wordsMap, wordWeights)
  
  expect_true(word %in% c("lainata", "nousta"))
})

test_that("sampleWord respects higher weights for words with multiple translations", {
  wordsMap <- list(
    "word1" = c("A", "B", "C"),
    "word2" = c("D"),
    "word3" = c("E")
  )
  wordWeights <- list("word1" = 10, "word2" = 1, "word3" = 1)
  
  set.seed(42)
  samples <- replicate(1000, sampleWord(wordsMap, wordWeights))
  
  word1_count <- sum(samples == "word1")
  expect_gt(word1_count, 700)
})

test_that("calculateSampleProbability calculates correct percentage for equal weights", {
  wordWeights <- list("alpha" = 1, "beta" = 1, "gamma" = 1)
  
  prob <- calculateSampleProbability("alpha", wordWeights)
  
  expect_equal(prob, 33.33)
})

test_that("calculateSampleProbability calculates correct percentage for different weights", {
  wordWeights <- list("alpha" = 10, "beta" = 1, "gamma" = 1)
  
  prob_alpha <- calculateSampleProbability("alpha", wordWeights)
  prob_beta <- calculateSampleProbability("beta", wordWeights)
  
  expect_equal(prob_alpha, 83.33)
  expect_equal(prob_beta, 8.33)
})

test_that("calculateSampleProbability returns 100% for single word", {
  wordWeights <- list("alpha" = 5)
  
  prob <- calculateSampleProbability("alpha", wordWeights)
  
  expect_equal(prob, 100)
})

test_that("calculateSampleProbability handles very high weights", {
  wordWeights <- list("alpha" = 1000, "beta" = 1)
  
  prob_alpha <- calculateSampleProbability("alpha", wordWeights)
  prob_beta <- calculateSampleProbability("beta", wordWeights)
  
  expect_gt(prob_alpha, 99.8)
  expect_lt(prob_beta, 0.2)
})

test_that("calculateSampleProbability returns a numeric value", {
  wordWeights <- list("alpha" = 5, "beta" = 3)
  
  prob <- calculateSampleProbability("alpha", wordWeights)
  
  expect_type(prob, "double")
})

test_that("calculateSampleProbability rounds to 2 decimal places", {
  wordWeights <- list("alpha" = 1, "beta" = 1, "gamma" = 1)
  
  prob <- calculateSampleProbability("alpha", wordWeights)
  
  # Check that it has at most 2 decimal places
  prob_str <- as.character(prob)
  decimal_part <- strsplit(prob_str, "\\.")[[1]]
  if (length(decimal_part) > 1) {
    expect_lte(nchar(decimal_part[2]), 2)
  }
})

test_that("probabilities sum to approximately 100%", {
  wordWeights <- list("alpha" = 5, "beta" = 3, "gamma" = 2)
  
  prob_alpha <- calculateSampleProbability("alpha", wordWeights)
  prob_beta <- calculateSampleProbability("beta", wordWeights)
  prob_gamma <- calculateSampleProbability("gamma", wordWeights)
  
  total <- prob_alpha + prob_beta + prob_gamma
  
  # Due to rounding, might be 99.99 or 100.01
  expect_gte(total, 99.9)
  expect_lte(total, 100.1)
})

test_that("sampled words have calculable probabilities", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"), "gamma" = c("C"))
  wordWeights <- list("alpha" = 5, "beta" = 3, "gamma" = 2)
  
  set.seed(42)
  word <- sampleWord(wordsMap, wordWeights)
  prob <- calculateSampleProbability(word, wordWeights)
  
  expect_gt(prob, 0)
  expect_lte(prob, 100)
})

test_that("higher probability words are sampled more often", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("alpha" = 9, "beta" = 1)
  
  prob_alpha <- calculateSampleProbability("alpha", wordWeights)
  prob_beta <- calculateSampleProbability("beta", wordWeights)
  
  expect_gt(prob_alpha, prob_beta)
  
  set.seed(42)
  samples <- replicate(1000, sampleWord(wordsMap, wordWeights))
  alpha_count <- sum(samples == "alpha")
  beta_count <- sum(samples == "beta")
  
  expect_gt(alpha_count, beta_count)
})
