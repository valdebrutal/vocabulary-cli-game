library(rjson)
library(argparser, quietly = TRUE)
source("src/parseUserInput.R")
source("src/stats.R")
source("src/cli.R")
source("src/wordMap.R")
source("src/validation.R")
source("src/sampling.R")
source("src/game.R")


argvals <- parse_args(argParser())
parsedArgs <- validateArgs(argvals)

wordsMap <-
  rjson::fromJSON(file = file.path("resources", paste0(parsedArgs$wordlist, ".json")))

cacheFile <- paste0(parsedArgs$wordlist, ".rds")
cacheFile <-
  ifelse(parsedArgs$reverse, paste0("reverse_", cacheFile) , cacheFile)
serializedWordWeightsFile <- file.path(".cache",  cacheFile)

if(parsedArgs$reverse){
  wordsMap<- reverseWordMap(wordsMap)
}
loadWordWeights(serializedWordWeightsFile, wordsMap, stats)

sorted <- sortWordMapAndWeights(wordsMap, stats$wordWeights)
wordsMap <- sorted$wordsMap
stats$wordWeights <- sorted$wordWeights
validateWordMapAndWeights(wordsMap, stats$wordWeights)

stdIn <- file("stdin")

while (TRUE) {
  wordToBeAsked <- sampleWord(wordsMap, stats$wordWeights)
  probability <- calculateSampleProbability(wordToBeAsked, stats$wordWeights)
  promptForTranslation(wordToBeAsked, probability)
  
  userAnswer <- getUserInput(stdIn)
  
  if (length(userAnswer) > 0 && userAnswer == "summary") {
    handleSummaryCommand(stats)
    next
  }
  
  if (length(userAnswer) > 0 && userAnswer == "exit") {
    handleExitCommand(stats, serializedWordWeightsFile)
  }
  
  processAnswer(userAnswer, wordToBeAsked, wordsMap, stats)
}
