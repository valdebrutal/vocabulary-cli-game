
test_that("validateWordMapAndWeights passes when lengths match and names match", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("alpha" = 1, "beta" = 2)
  
  expect_silent(validateWordMapAndWeights(wordsMap, wordWeights))
})

test_that("validateWordMapAndWeights throws error when lengths don't match", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("alpha" = 1, "beta" = 2, "gamma" = 3)
  
  expect_error(
    validateWordMapAndWeights(wordsMap, wordWeights),
    "Length mismatch"
  )
})

test_that("validateWordMapAndWeights throws error when names don't match", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("alpha" = 1, "gamma" = 2)
  
  expect_error(
    validateWordMapAndWeights(wordsMap, wordWeights),
    "don't match"
  )
})

test_that("validateWordMapAndWeights throws error when names are in different order", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("beta" = 2, "alpha" = 1)
  
  expect_error(
    validateWordMapAndWeights(wordsMap, wordWeights),
    "don't match"
  )
})

test_that("validateWordMapAndWeights handles empty lists", {
  wordsMap <- list()
  wordWeights <- list()
  
  expect_silent(validateWordMapAndWeights(wordsMap, wordWeights))
})

test_that("validateWordMapAndWeights handles single entry", {
  wordsMap <- list("alpha" = c("A"))
  wordWeights <- list("alpha" = 5)
  
  expect_silent(validateWordMapAndWeights(wordsMap, wordWeights))
})

test_that("sortWordMapAndWeights sorts both maps by names", {
  wordsMap <- list("gamma" = c("C"), "alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("gamma" = 3, "alpha" = 1, "beta" = 2)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_equal(names(sorted$wordsMap), c("alpha", "beta", "gamma"))
  expect_equal(names(sorted$wordWeights), c("alpha", "beta", "gamma"))
})

test_that("sortWordMapAndWeights returns a list with wordsMap and wordWeights", {
  wordsMap <- list("alpha" = c("A"))
  wordWeights <- list("alpha" = 1)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_true("wordsMap" %in% names(sorted))
  expect_true("wordWeights" %in% names(sorted))
})

test_that("sortWordMapAndWeights handles already sorted maps", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"), "gamma" = c("C"))
  wordWeights <- list("alpha" = 1, "beta" = 2, "gamma" = 3)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_equal(names(sorted$wordsMap), c("alpha", "beta", "gamma"))
  expect_equal(names(sorted$wordWeights), c("alpha", "beta", "gamma"))
})

test_that("sortWordMapAndWeights handles maps with special characters in names", {
  wordsMap <- list("ö" = c("O"), "ä" = c("A"), "a" = c("a"))
  wordWeights <- list("ö" = 3, "ä" = 2, "a" = 1)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  # After sorting, check they have same order
  expect_equal(names(sorted$wordsMap), names(sorted$wordWeights))
})

test_that("sortWordMapAndWeights handles single element", {
  wordsMap <- list("alpha" = c("A"))
  wordWeights <- list("alpha" = 1)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_equal(names(sorted$wordsMap), "alpha")
  expect_equal(names(sorted$wordWeights), "alpha")
})

test_that("sortWordMapAndWeights handles empty lists", {
  wordsMap <- list()
  wordWeights <- list()
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_true(is.list(sorted$wordsMap))
  expect_true(is.list(sorted$wordWeights))
})

test_that("sorted maps pass validation", {
  wordsMap <- list("gamma" = c("C"), "alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("beta" = 2, "gamma" = 3, "alpha" = 1)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_silent(validateWordMapAndWeights(sorted$wordsMap, sorted$wordWeights))
})

test_that("detects mismatches even after sorting", {
  wordsMap <- list("alpha" = c("A"), "beta" = c("B"))
  wordWeights <- list("alpha" = 1, "gamma" = 2)
  
  sorted <- sortWordMapAndWeights(wordsMap, wordWeights)
  
  expect_error(
    validateWordMapAndWeights(sorted$wordsMap, sorted$wordWeights),
    "don't match"
  )
})
