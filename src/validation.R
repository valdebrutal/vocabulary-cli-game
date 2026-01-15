# Validation functions for word maps and weights

validateWordMapAndWeights <- function(wordsMap, wordWeights) {
  # Check if lengths match
  if (length(wordsMap) != length(wordWeights)) {
    stop(sprintf(
      "Length mismatch: wordsMap has %d entries but wordWeights has %d. This should not happen after cleanup.",
      length(wordsMap),
      length(wordWeights)
    ))
  }

  # Check if names match after sorting
  if (!all(names(wordsMap) == names(wordWeights))) {
    stop(
      "Word names in wordsMap and wordWeights don't match after sorting. This should not happen."
    )
  }

  invisible(TRUE)
}

sortWordMapAndWeights <- function(wordsMap, wordWeights) {
  # Handle empty lists
  if (length(wordsMap) == 0 || length(wordWeights) == 0) {
    return(list(
      wordsMap = wordsMap,
      wordWeights = wordWeights
    ))
  }

  list(
    wordsMap = wordsMap[order(names(wordsMap))],
    wordWeights = wordWeights[order(names(wordWeights))]
  )
}
