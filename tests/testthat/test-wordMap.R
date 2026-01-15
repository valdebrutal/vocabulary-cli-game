test_that("loadWordWeights initializes weights to 1 when no cache exists", {
  stats <- list2env(list(wordWeights = list()))
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))

  loadWordWeights("nonexistent_cache.rds", wordsMap, stats)

  expect_length(stats$wordWeights, 2)
  expect_equal(stats$wordWeights[["alpha"]], 1)
  expect_equal(stats$wordWeights[["beta"]], 1)
})

test_that("loadWordWeights loads weights from cache when it exists", {
  stats <- list2env(list(wordWeights = list()))
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))

  # Create a cache file
  cacheFile <- tempfile(fileext = ".rds")
  cachedWeights <- list("alpha" = 5, "beta" = 3)
  saveRDS(cachedWeights, cacheFile)

  loadWordWeights(cacheFile, wordsMap, stats)

  expect_equal(stats$wordWeights[["alpha"]], 5)
  expect_equal(stats$wordWeights[["beta"]], 3)

  unlink(cacheFile)
})

test_that("loadWordWeights removes stale weights from cache", {
  stats <- list2env(list(wordWeights = list()))
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))

  # Cache contains an extra word "gamma" that's not in wordsMap
  cacheFile <- tempfile(fileext = ".rds")
  cachedWeights <- list("alpha" = 5, "beta" = 3, "gamma" = 10)
  saveRDS(cachedWeights, cacheFile)

  loadWordWeights(cacheFile, wordsMap, stats)

  expect_length(stats$wordWeights, 2)
  expect_true("alpha" %in% names(stats$wordWeights))
  expect_true("beta" %in% names(stats$wordWeights))
  expect_false("gamma" %in% names(stats$wordWeights))

  unlink(cacheFile)
})

test_that("loadWordWeights adds new words with weight 1", {
  stats <- list2env(list(wordWeights = list()))
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"), "gamma" = c("C"))

  # Cache only has alpha and beta
  cacheFile <- tempfile(fileext = ".rds")
  cachedWeights <- list("alpha" = 5, "beta" = 3)
  saveRDS(cachedWeights, cacheFile)

  loadWordWeights(cacheFile, wordsMap, stats)

  expect_length(stats$wordWeights, 3)
  expect_equal(stats$wordWeights[["alpha"]], 5)
  expect_equal(stats$wordWeights[["beta"]], 3)
  expect_equal(stats$wordWeights[["gamma"]], 1)

  unlink(cacheFile)
})

test_that("loadWordWeights handles cache with completely different words", {
  stats <- list2env(list(wordWeights = list()))
  wordsMap <- list("delta" = c("D"), "epsilon" = c("E"))

  # Cache has completely different words
  cacheFile <- tempfile(fileext = ".rds")
  cachedWeights <- list("alpha" = 5, "beta" = 3)
  saveRDS(cachedWeights, cacheFile)

  loadWordWeights(cacheFile, wordsMap, stats)

  expect_length(stats$wordWeights, 2)
  expect_equal(stats$wordWeights[["delta"]], 1)
  expect_equal(stats$wordWeights[["epsilon"]], 1)
  expect_false("alpha" %in% names(stats$wordWeights))
  expect_false("beta" %in% names(stats$wordWeights))

  unlink(cacheFile)
})

test_that("reverseWordMap reverses a simple one-to-one mapping", {
  wordMap <- list("alpha" = c("A"), "beta" = c("B"))

  reversed <- reverseWordMap(wordMap)

  expect_length(reversed, 2)
  expect_equal(reversed[["A"]], "alpha")
  expect_equal(reversed[["B"]], "beta")
})

test_that("reverseWordMap handles multiple words with same translation", {
  wordMap <- list(
    "pelata" = c("play"),
    "soittaa" = c("play"),
    "leikkiä" = c("play")
  )

  reversed <- reverseWordMap(wordMap)

  expect_length(reversed, 1)
  expect_length(reversed[["play"]], 3)
  expect_true("pelata" %in% reversed[["play"]])
  expect_true("soittaa" %in% reversed[["play"]])
  expect_true("leikkiä" %in% reversed[["play"]])
})

test_that("reverseWordMap handles words with multiple translations", {
  wordMap <- list(
    "lainata" = c("lend", "borrow"),
    "nousta" = c("get up", "increase", "arise")
  )

  reversed <- reverseWordMap(wordMap)

  expect_length(reversed, 5)
  expect_equal(reversed[["lend"]], "lainata")
  expect_equal(reversed[["borrow"]], "lainata")
  expect_equal(reversed[["get up"]], "nousta")
  expect_equal(reversed[["increase"]], "nousta")
  expect_equal(reversed[["arise"]], "nousta")
})

test_that("reverseWordMap handles complex many-to-many relationships", {
  wordMap <- list(
    "word1" = c("A", "B"),
    "word2" = c("B", "C"),
    "word3" = c("A")
  )

  reversed <- reverseWordMap(wordMap)

  expect_length(reversed, 3)
  expect_length(reversed[["A"]], 2)
  expect_length(reversed[["B"]], 2)
  expect_length(reversed[["C"]], 1)
  expect_true("word1" %in% reversed[["A"]])
  expect_true("word3" %in% reversed[["A"]])
  expect_true("word1" %in% reversed[["B"]])
  expect_true("word2" %in% reversed[["B"]])
})

test_that("reverseWordMap handles empty word map", {
  wordMap <- list()

  reversed <- reverseWordMap(wordMap)

  expect_length(reversed, 0)
})
