validateArgs <- function(args) {
  wordLists <- tools::file_path_sans_ext(list.files("resources"))
  if (length(args) == 0 || !args$wordlist %in% wordLists) {
    message(
      sprintf(
        "You need to specify the wordlist argument whose value can be one of: %s. It signals the type of words that will be asked. Example: make run wordlist=verbs",
        paste(wordLists, collapse = ", ")
      )
    )
    quit(status = 1, save = "no")
  }
  args
}

argParser <- function() {
  argParser <-
    arg_parser(
      name = "Translation game",
      description = "Skip a word pressing enter and get stats on how you're doing typing summary and pressing enter.",
      hide.opts = T
    )
  argParser <-
    add_argument(
      argParser,
      "wordlist",
      help = "Name of the file containing the corpus of words to play with."
    )
  add_argument(
    argParser,
    "--reverse",
    help = "Reverse the corpus direction of the translation. For instance, if the keys are finnish words and the translations are english, this flag will make the keys english and the translations finnish.",
    flag = TRUE
  )
}
