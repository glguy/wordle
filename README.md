# Console-based Wordle Implementation

This implementation can solve wordle puzzles or let you play wordle puzzles.

I'm not distributing a word list as I don't know that I have license to do so.
The game uses two word lists:

* `all.txt` for all the valid words
* `play.txt` for a list of words to randomly select from as the target word

## Usage

The application runs in one of three modes and has configurable letter view and
computer search strategy.

Key bindings:

* <kbd>Enter</kbd> - Submit word
* <kbd>Backspace</kbd> - Delete last input
* <kbd>?</kbd> - Ask computer to select a word
* <kbd>Ctrl</kbd><kbd>C</kbd> - Quit

Solve mode bindings:

* <kbd>B</kbd> - Mark a letter black - This letter is not in the target word
* <kbd>Y</kbd> - Mark a letter yellow - This letter is in the target word, but not at this location
* <kbd>G</kbd> - Mark a letter green - This letter is correct for this location

```
Usage: wordle [FLAGS] MODE

Modes:
    solve - Program guesses a secret word, reply with 'b' 'y' 'g'
    play  - Program picks a random word, type in your guess words
    give  - User types in the secret words, then types in guess words

    --dict=FILE     Dictionary
    --words=FILE    Word list
    --worstcase     Strategy: worst case
    --maxentropy    Strategy: maximum entropy
    --sumofsquares  Strategy: sum of squares
    --mostchoices   Strategy: most choices (default)
    --easy          Disable hard mode (default)
    --hard          Enable hard mode
    --qwerty        Keyboard layout: qwerty (default)
    --dvorak        Keyboard layout: dvorak
    --colemak       Keyboard layout: colemak
    --alphabet      Keyboard layout: alphabet
    --frequencies   Keyboard layout: frequencies
```


## Demonstration

See wordle in action:

[![asciicast](https://asciinema.org/a/XgXUnc7RAPRAlYOH5TOCmR9fx.svg)](https://asciinema.org/a/XgXUnc7RAPRAlYOH5TOCmR9fx)
