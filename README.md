# Major mode interface to WordNet

Uses wm(1) for searching local wordnet db; injects results in a
org-mode derived buffer.

## Features

* 1 buffer `*WordNet*` for all query results.
* Ido suggestions if wm finds the query too ambiguous.
* Back/forward/view history.

## Installation

	# dnf install wordnet

In `~/.emacs`:

	(add-to-list 'load-path "/the/dir/with/the/repo")
	(require 'wn-org2)

## Keyboard shortcuts

There is no default global keybindings. Add something like:

	(global-set-key [f12] 'wn-org2-search)
	(global-set-key [(control f12)] 'wn-org2-lookup-current-word)

to begin with.

In the `*WordNet*` buffer:

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
mode. wn-org2 still contains the buffer formatting code from it.

## License

GPLv2.
