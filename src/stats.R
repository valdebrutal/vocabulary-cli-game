stats <- list2env(
  list(
    rightAnswers = 0L,
    wrongAnswers = 0L,
    totalAnswers = 0L,
    wordWeights = list(),
    formatStats = function() {
      baseMesssage <- sprintf(
        "  --> Total Answers: %d.\n  --> Right Answers: %d\n  --> %% Correct: %.1f%%\n --> Corpus size: %d\n",
        stats$totalAnswers,
        stats$rightAnswers,
        round(
          ifelse(
            stats$totalAnswers == 0,
            0,
            stats$rightAnswers / stats$totalAnswers
          ),
          3
        ) *
          100,
        length(stats$wordWeights)
      )
      strugglingWords <- stats$wordWeights[stats$wordWeights > 1]
      if (length(strugglingWords) > 0) {
        top5 <-
          unlist(strugglingWords[order(
            unlist(strugglingWords),
            decreasing = T
          )][1:5]) # unlist drops nulls
        top5WithMistakes <-
          sapply(names(top5), function(word) {
            paste0(word, " (", top5[[word]], ")")
          })
        top5Message <-
          paste0(
            "The top 5 words you're struggling the most with are (cumulative mistakes in parenthesis): ",
            paste(top5WithMistakes, collapse = ", ")
          )
        baseMesssage <- paste0(baseMesssage, top5Message)
      }
      baseMesssage
    },
    registerMistake = function(word) {
      stats$wrongAnswers <- stats$wrongAnswers + 1
      stats$wordWeights[[word]] <- stats$wordWeights[[word]] + 1
    },
    registerCorrectAnswer = function(word) {
      stats$rightAnswers <- stats$rightAnswers + 1
      stats$wordWeights[[word]] <-
        max(stats$wordWeights[[word]] - 1, 1)
    },
    recordAnswer = function(word, correct) {
      stats$totalAnswers <- stats$totalAnswers + 1
      if (correct) {
        stats$registerCorrectAnswer(word)
      } else {
        stats$registerMistake(word)
      }
    }
  )
)
