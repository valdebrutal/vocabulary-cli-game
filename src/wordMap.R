loadWordWeights <-
  function(serializedWordWeightsFile,
           wordMap,
           stats) {
    if (file.exists(serializedWordWeightsFile)) {
      message(paste0("Loading cache at: ", serializedWordWeightsFile))
      deserializedWordWeights <- readRDS(serializedWordWeightsFile)
      
      # Remove stale weights for words that no longer exist in wordMap
      deserializedWordWeights <-
        deserializedWordWeights[names(deserializedWordWeights) %in% names(wordMap)]
      
      wordsNotInCache <-
        names(wordMap)[!names(wordMap) %in% names(deserializedWordWeights)]
      stats$wordWeights <-
        c(
          deserializedWordWeights,
          sapply(wordsNotInCache, function(word)
            1, simplify = F, USE.NAMES = T)
        )
    } else {
      stats$wordWeights <- lapply(wordMap, function(word)
        1)
    }
  }

reverseWordMap <- function(wordMap) {
  newMap <- list()
  for (key in names(wordMap)) {
    for (translation in wordMap[[key]]) {
      newMap[[translation]] <- c(newMap[[translation]], key)
    }
  }
  newMap
}