parseUserInput <- function(rawInput) {
  tolower(trimws(iconv(enc2utf8(rawInput), sub = "byte"), which = "both"))
}
