# word-count.el
`word-count.el` is an Emacs minor mode that tracks the number of words typed in real-time within any activated buffer. It records the word count daily, organized by instance, enabling you to monitor your writing progress effectively.

## Features

- Counts words in real-time as you type.
- Logs daily word count to a specified file in a TSV format.
- Hooks for custom functions to be executed on word count updates and before saving.
- Easily customizable log file path.
- Safe to use with multiple simultaneous Emacs instances.

## Installation

You can clone this repository and load it manually by following these steps:

```sh
git clone https://github.com/brunocbr/word-count ~/.emacs.d/lisp/word-count
```

Then, add the following to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/word-count")
(require 'word-count)
```

## Usage

To enable `word-count-mode`, simply run the command:

```elisp
M-x word-count-enable
```

You can also enable it automatically in `text-mode` (or other modes) by adding the following to your Emacs configuration:

```elisp
(add-hook 'text-mode-hook 'word-count-mode)
```

## Customize Logging Files Location

You can customize the location of your log files by setting the `word-count-log-files` variable:

```elisp
(setq word-count-log-files "~/path/to/your/word-count-log-%s-%s.txt")
```

The required `%s` placeholders will be replaced by the date and a unique identifier for the Emacs instance, respectively.

### Hooks

You can add your own functions to run when the word count updates. For example, to display a motivational message every 200 words written:

```elisp
(add-hook 'word-count-count-words-hook
          (lambda ()
            (when (zerop (% (word-count-total-words-today) 200))
              (message "Great job! You've written %d words today!" word-count-my-word-count))))
```

## License

This project is licensed under the GNU General Public License v3.0.

