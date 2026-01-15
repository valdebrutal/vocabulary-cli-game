library(testthat)
source("src/parseUserInput.R")
source("src/stats.R")
source("src/wordMap.R")
source("src/validation.R")
source("src/sampling.R")
source("src/game.R")

test_dir("tests/testthat", reporter = "progress")
