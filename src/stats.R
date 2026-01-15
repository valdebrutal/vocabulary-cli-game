#' Create a new game stats object
#'
#' @return A game_stats S3 object
new_stats <- function() {
  structure(
    list(
      rightAnswers = 0L,
      wrongAnswers = 0L,
      totalAnswers = 0L,
      wordWeights = list()
    ),
    class = "game_stats"
  )
}

#' Format game stats for display
#'
#' @param x A game_stats object
#' @param ... Additional arguments (unused)
#' @return A formatted string
format.game_stats <- function(x, ...) {
  baseMessage <- sprintf(
    "  --> Total Answers: %d.\n  --> Right Answers: %d\n  --> %% Correct: %.1f%%\n --> Corpus size: %d\n",
    x$totalAnswers,
    x$rightAnswers,
    round(
      ifelse(
        x$totalAnswers == 0,
        0,
        x$rightAnswers / x$totalAnswers
      ),
      3
    ) *
      100,
    length(x$wordWeights)
  )

  strugglingWords <- x$wordWeights[x$wordWeights > 1]
  if (length(strugglingWords) > 0) {
    top5 <-
      unlist(strugglingWords[order(
        unlist(strugglingWords),
        decreasing = TRUE
      )][1:5]) # unlist drops nulls

    top5WithMistakes <-
      sapply(names(top5), function(word) {
        paste0(word, " (", top5[[word]] - 1, ")") # -1 because weights start at 1
      })

    top5Message <-
      paste0(
        "The top 5 words you're struggling the most with are (cumulative mistakes in parenthesis): ",
        paste(top5WithMistakes, collapse = ", ")
      )

    baseMessage <- paste0(baseMessage, top5Message)
  }

  baseMessage
}

#' Print game stats
#'
#' @param x A game_stats object
#' @param ... Additional arguments (unused)
print.game_stats <- function(x, ...) {
  cat(format(x), "\n")
  invisible(x)
}

#' Register a mistake for a word
#'
#' @param stats A game_stats object
#' @param word The word that was answered incorrectly
#' @return Modified game_stats object
register_mistake <- function(stats, word) {
  stats$wrongAnswers <- stats$wrongAnswers + 1
  stats$wordWeights[[word]] <- stats$wordWeights[[word]] + 1
  stats
}

#' Register a correct answer for a word
#'
#' @param stats A game_stats object
#' @param word The word that was answered correctly
#' @return Modified game_stats object
register_correct_answer <- function(stats, word) {
  stats$rightAnswers <- stats$rightAnswers + 1
  stats$wordWeights[[word]] <- max(stats$wordWeights[[word]] - 1, 1)
  stats
}

#' Record an answer (correct or incorrect)
#'
#' @param stats A game_stats object
#' @param word The word that was asked
#' @param correct Whether the answer was correct
#' @return Modified game_stats object
record_answer <- function(stats, word, correct) {
  stats$totalAnswers <- stats$totalAnswers + 1

  if (correct) {
    stats <- register_correct_answer(stats, word)
  } else {
    stats <- register_mistake(stats, word)
  }

  stats
}
