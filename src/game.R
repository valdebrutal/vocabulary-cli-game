# Game session management functions

promptForTranslation <- function(word, probability) {
  message(sprintf(
    "Do translate the word %s [p=%s] (or press enter to skip)",
    toupper(word),
    paste0(probability, "%")
  ))
}

getUserInput <- function(stdIn) {
  parseUserInput(
    scan(
      file = stdIn,
      what = "character",
      n = 1,
      nlines = 1,
      quiet = TRUE,
      sep = "\n",
      skipNul = TRUE,
      skip = 0
    )
  )
}

handleSummaryCommand <- function(stats) {
  message(stats$formatStats())
  return(TRUE)
}

handleExitCommand <- function(stats, cacheFile) {
  message("Quitting! Remember: Practice makes perfect.")
  message("Performance:")
  message(stats$formatStats())
  saveRDS(stats$wordWeights, file = cacheFile)
  closeAllConnections()
  quit(save = "no")
}

checkAnswer <- function(userAnswer, correctAnswers) {
  if (length(userAnswer) == 0) {
    return(list(correct = FALSE, skipped = TRUE))
  }
  
  correct <- userAnswer %in% correctAnswers
  return(list(correct = correct, skipped = FALSE))
}

provideFeedback <- function(result, userAnswer, correctAnswers) {
  if (result$correct) {
    message("Correct! :D")
  } else if (result$skipped) {
    correctText <- paste(toupper(correctAnswers), collapse = " or ")
    message(sprintf("Skipping! It means: '%s'.", correctText))
  } else {
    correctText <- paste(toupper(correctAnswers), collapse = " or ")
    message(sprintf(
      "Wrong! The right answer is NOT '%s', but '%s'.",
      userAnswer,
      correctText
    ))
  }
}

processAnswer <- function(userAnswer, word, wordsMap, stats) {
  correctAnswers <- wordsMap[[word]]
  result <- checkAnswer(userAnswer, correctAnswers)
  
  stats$recordAnswer(word, result$correct)
  provideFeedback(result, userAnswer, correctAnswers)
  
  return(result$correct)
}
