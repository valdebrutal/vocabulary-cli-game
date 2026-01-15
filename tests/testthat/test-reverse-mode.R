
test_that("reverse mode creates valid reversed word map", {
  # Original: Finnish -> English
  original <- list(
    "yksi" = c("one"),
    "kaksi" = c("two"),
    "kolme" = c("three")
  )
  
  # Reverse: English -> Finnish
  reversed <- reverseWordMap(original)
  
  expect_equal(reversed[["one"]], "yksi")
  expect_equal(reversed[["two"]], "kaksi")
  expect_equal(reversed[["three"]], "kolme")
})

test_that("reverse mode handles multiple translations correctly", {
  # Finnish words with same English translation
  original <- list(
    "pelata" = c("play"),
    "soittaa" = c("play"),
    "leikkiä" = c("play")
  )
  
  reversed <- reverseWordMap(original)
  
  expect_length(reversed, 1)
  expect_length(reversed[["play"]], 3)
  expect_true("pelata" %in% reversed[["play"]])
  expect_true("soittaa" %in% reversed[["play"]])
  expect_true("leikkiä" %in% reversed[["play"]])
})

test_that("reverse mode with multiple translations per word", {
  # One Finnish word with multiple English translations
  original <- list(
    "lainata" = c("lend", "borrow")
  )
  
  reversed <- reverseWordMap(original)
  
  expect_length(reversed, 2)
  expect_equal(reversed[["lend"]], "lainata")
  expect_equal(reversed[["borrow"]], "lainata")
})

test_that("reversed map passes validation after sorting", {
  original <- list(
    "alpha" = c("A"),
    "beta" = c("B", "BB"),
    "gamma" = c("C")
  )
  
  reversed <- reverseWordMap(original)
  weights <- lapply(reversed, function(x) 1)
  
  sorted <- sortWordMapAndWeights(reversed, weights)
  
  expect_silent(validateWordMapAndWeights(sorted$wordsMap, sorted$wordWeights))
})

test_that("reverse mode preserves all information", {
  original <- list(
    "word1" = c("A", "B"),
    "word2" = c("B", "C"),
    "word3" = c("D")
  )
  
  reversed <- reverseWordMap(original)
  
  # Check all translations are present
  expect_length(reversed, 4)  # A, B, C, D
  expect_true("A" %in% names(reversed))
  expect_true("B" %in% names(reversed))
  expect_true("C" %in% names(reversed))
  expect_true("D" %in% names(reversed))
  
  # Check reverse mappings
  expect_equal(reversed[["A"]], "word1")
  expect_length(reversed[["B"]], 2)
  expect_true("word1" %in% reversed[["B"]])
  expect_true("word2" %in% reversed[["B"]])
})

test_that("reverse mode cache uses different filename", {
  # This tests the logic in main.R lines 15-17
  # When reverse=TRUE, cache file should be "reverse_wordlist.rds"
  
  wordlist <- "verbs"
  reverse <- TRUE
  
  cacheFile <- paste0(wordlist, ".rds")
  cacheFile <- ifelse(reverse, paste0("reverse_", cacheFile), cacheFile)
  
  expect_equal(cacheFile, "reverse_verbs.rds")
})

test_that("reverse mode cache doesn't conflict with normal mode", {
  # Normal mode
  wordlist <- "verbs"
  reverse <- FALSE
  
  cacheFile <- paste0(wordlist, ".rds")
  cacheFile <- ifelse(reverse, paste0("reverse_", cacheFile), cacheFile)
  
  expect_equal(cacheFile, "verbs.rds")
  
  # Reverse mode
  reverse <- TRUE
  cacheFile <- paste0(wordlist, ".rds")
  cacheFile <- ifelse(reverse, paste0("reverse_", cacheFile), cacheFile)
  
  expect_equal(cacheFile, "reverse_verbs.rds")
  
  # They should be different
  expect_false("verbs.rds" == "reverse_verbs.rds")
})

test_that("reversed map works with sampling", {
  original <- list(
    "alpha" = c("A"),
    "beta" = c("B"),
    "gamma" = c("C")
  )
  
  reversed <- reverseWordMap(original)
  weights <- list("A" = 10, "B" = 1, "C" = 1)
  
  # Sort to ensure alignment
  sorted <- sortWordMapAndWeights(reversed, weights)
  
  # Should be able to sample
  set.seed(42)
  word <- sampleWord(sorted$wordsMap, sorted$wordWeights)
  
  expect_true(word %in% names(sorted$wordsMap))
  
  # With high weight on "A", it should be sampled more
  set.seed(42)
  samples <- replicate(1000, sampleWord(sorted$wordsMap, sorted$wordWeights))
  
  expect_gt(sum(samples == "A"), 700)
})

test_that("reverse mode handles complex real-world data", {
  # Simulate real Finnish verb data
  original <- list(
    "nousta" = c("get up", "increase", "arise", "rise", "stand up"),
    "lainata" = c("lend", "borrow"),
    "pelata" = c("play"),
    "soittaa" = c("play"),
    "leikkiä" = c("play")
  )
  
  reversed <- reverseWordMap(original)
  
  # "play" should map to 3 Finnish words
  expect_length(reversed[["play"]], 3)
  
  # "get up" should map to 1 Finnish word
  expect_equal(reversed[["get up"]], "nousta")
  
  # All 5 translations of "nousta" should be keys
  expect_true("get up" %in% names(reversed))
  expect_true("increase" %in% names(reversed))
  expect_true("arise" %in% names(reversed))
  expect_true("rise" %in% names(reversed))
  expect_true("stand up" %in% names(reversed))
})

test_that("double reverse returns to original structure", {
  original <- list(
    "alpha" = c("A"),
    "beta" = c("B"),
    "gamma" = c("C")
  )
  
  reversed_once <- reverseWordMap(original)
  reversed_twice <- reverseWordMap(reversed_once)
  
  # After double reverse, structure should match original
  # (though order might differ)
  expect_equal(sort(names(reversed_twice)), sort(names(original)))
  expect_equal(reversed_twice[["alpha"]], original[["alpha"]])
  expect_equal(reversed_twice[["beta"]], original[["beta"]])
  expect_equal(reversed_twice[["gamma"]], original[["gamma"]])
})
