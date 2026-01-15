sampleWord <- function(wordsMap, wordWeights) {
  names(sample(wordsMap, size = 1, prob = unlist(wordWeights)))
}

calculateSampleProbability <- function(word, wordWeights) {
  # Calculate the probability percentage for a given word
  round(wordWeights[[word]] / sum(unlist(wordWeights)) * 100, 2)
}
