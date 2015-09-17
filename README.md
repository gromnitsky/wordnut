# Major mode interface to WordNet

Uses wn(1) for searching local wordnet db; injects results in a
outline-mode derived buffer.

## Features

* Nothing to configure (except for optional custom keybindings).
* **Completion** if wn(1) finds the query too ambiguous.
* 1 buffer `*WordNut*` for all query results.
* Back/forward/view history.

![A screenshot of running wordnut](https://raw.github.com/gromnitsky/wordnut/master/screenshot1.png)

## Installation

	# dnf install wordnet

In `~/.emacs`:

	(add-to-list 'load-path "/the/dir/with/the/repo")
	(require 'wordnut)

If you have
[adaptive-wrap](http://elpa.gnu.org/packages/adaptive-wrap.html) mode
installed, wordnut will automatically use it to improve the text
formatting.

## Keyboard shortcuts

There is no default global keybindings. Add something like:

	(global-set-key [f12] 'wordnut-search)
	(global-set-key [(control f12)] 'wordnut-lookup-current-word)

to begin with.

In the `*WordNut*` buffer:

kbd               | desc
----------------- | -------------
<kbd>Enter</kbd>  | Lookup a word under the cursor
<kbd>o</kbd>      | A tooltip w/ a `sense` for the current `lexical category`
<kbd>/</kbd>      | New search
<kbd>l</kbd>      | Move backward in history
<kbd>r</kbd>      | Move forward in history
<kbd>h</kbd>      | View history
<kbd>q</kbd>      | Hide buffer

Auxiliary:

kbd                                  | desc
------------------------------------ | -------------
<kbd>Tab</kbd>                       | Toggle heading/section visibility
<kbd>M-Up</kbd>, <kbd>M-Down</kbd>   | Move between sections
<kbd>Space</kbd>                     | PageDown
<kbd>b</kbd>, <kbd>Backspace</kbd>   | PageUp

## Credits

The inspiration was [wn-org.el](http://emacswiki.org/emacs/wn-org.el)
mode.

## Bugs

* ≈ 14KB .el size is too much for such a small major mode.
* Tested only w/ wordnet-3.0 on Fedora 22.

## TODO

* Display a lexical category in the echo area akin to eldoc.
* Expand inline references to another word.

## License

GPLv2.
