# Major mode interface to WordNet

Uses wn(1) for searching local wordnet db; injects results in a
outline-mode derived buffer.

## Features

* Nothing to configure (except for optional custom keybindings).
* 1 buffer `*WordNut*` for all query results.
* Ido suggestions if wn(1) finds the query too ambiguous.
* Back/forward/view history.

![A screenshot of running wordnut](https://raw.github.com/gromnitsky/wordnut/master/screnshot1.png)

## Installation

	# dnf install wordnet

In `~/.emacs`:

	(add-to-list 'load-path "/the/dir/with/the/repo")
	(require 'wordnut)

## Keyboard shortcuts

There is no default global keybindings. Add something like:

	(global-set-key [f12] 'wordnut-search)
	(global-set-key [(control f12)] 'wordnut-lookup-current-word)

to begin with.

In the `*WordNut*` buffer:

kbd               | desc
----------------- | -------------
<kbd>/</kbd>      | New search
<kbd>Enter</kbd>  | Lookup a word under the cursor
<kbd>l</kbd>      | Move backward in history
<kbd>r</kbd>      | Move forward in history
<kbd>h</kbd>      | View history
<kbd>q</kbd>      | Delete buffer

## Credits

The inspiration was [wn-org.el](http://emacswiki.org/emacs/wn-org.el)
mode.

## Bugs

* Tested only w/ wordnet-3.0 on Fedora 22.

## License

GPLv2.
