# CLI Vocabulary Game

This is just a CLI program that asks you to translate words based on a list of words where one or more translations are available. Found out that learning vocabulary this way is way more entertaining than any other ways I have tried.

The mistakes you make will increase the probability of being asked that same word again. Conversely, translating a word correctly will lessen the probability of that word being asked, although the probability never drops to 0.

## Usage

You'll need `docker` and optionally `make` installed. If you don't have `make` installed, you can also run the docker commands directly (pick them from the `Makefile`).

If it's the first time you use it, build the docker image:

```shell
make build
```

Run it selecting the word list you want to practice with (you can add your own word lists, see [Adding new words](#adding-new-words)). The word list needs to be available in the `resources` directory. For instance, for a list named `verbs.json` that'd be:

```shell
make run wordlist=verbs
```

The keys of the word list are the words that will be asked and the array of values are valid translations. It is possible to revert the direction of the translation (ie, to have the values become the keys and viceversa) by running the game like so:

```shell
make run_reverse wordlist=verbs
```

You can skip a given word you don't know by pressing enter. You will be told what's the right answer just like when you answer incorrectly.

To exit the program, type and enter `exit` when prompted for a translation. You'll get some quick stats on your performance.

### Adding new words

Just add the words on the right json file under `resources`. The translations are in an array in order to make available more than one single translation per word.

## Running Tests

### Using Make
```sh
make build && make test
```

### Standard testthat entry point
```sh
Rscript tests/testthat.R
```