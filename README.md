# Major mode interface to WordNet

Uses wn(1) for searching local wordnet db; injects results into
`*WordNut*` buffer.

## Features

* Nothing to configure (except for optional custom keybindings).
* **Completion** if wn(1) finds the query too ambiguous.
* 1 buffer `*WordNut*` for all query results.
* Back/forward/view history.

![A screenshot of running wordnut](https://raw.github.com/gromnitsky/wordnut/master/screenshot1.png)

## Requirements

* Emacs 24.4+
* wn(1) in `PATH`
* (optionally)
  [adaptive-wrap](http://elpa.gnu.org/packages/adaptive-wrap.html)

## Installation

If you have adaptive-wrap mode installed, wordnut will automatically
use it to improve the text formatting (don't forget to add `(require
'adaptive-wrap)` in `~/.emacs`).

### Fedora

	# dnf install wordnet

In `~/.emacs`:

	(add-to-list 'load-path "/the/dir/with/the/repo")
	(require 'wordnut)

### Windows

1. You need a compiled Windows version of Wordnet 3.0. Googling gives
   us a bizzare
   [WordNet 3.0 windows visual studio](http://sourceforge.net/projects/wordnet30forwin/files/WordNet%203.0/3.0.2/WordNet_3.0_win32.zip). Extract
   `bin` & `dict` directories from it to `c:\Program
   Files\WordNet\3.0\` (for some reason the path is hard-coded).

2. In `%APPDATA%\.emacs`:

		(if (eq 'windows-nt system-type)
			(progn
			  (add-to-list 'load-path "/the/dir/with/the/repo")
			  (setq wordnut-cmd "c:/Program Files/WordNet/3.0/bin/wn.exe")
			  (require 'wordnut)))

## Keyboard shortcuts

There is no default global keybindings. Add something like:

	(global-set-key [f12] 'wordnut-search)
	(global-set-key [(control f12)] 'wordnut-lookup-current-word)

to begin with.

In the `*WordNut*` buffer:

kbd                        | desc
-------------------------- | -------------
<kbd>Enter</kbd>           | Lookup a word under the cursor
<kbd>o</kbd>               | A tooltip w/ a _sense_ for the current _lexical category_
<kbd>/</kbd>               | New search
<kbd>l</kbd>, <kbd>r</kbd> | Move backward/forward in history
<kbd>h</kbd>               | View history
<kbd>q</kbd>               | Hide buffer

Auxiliary:

kbd                                  | desc
------------------------------------ | -------------
<kbd>M-Up</kbd>, <kbd>M-Down</kbd>   | Move between sections
<kbd>Space</kbd>                     | PageDown
<kbd>b</kbd>, <kbd>Backspace</kbd>   | PageUp

## Senses overview

When reading long entries it's easy to get lost in the number of word
_senses_. For example, do `M-x wordnut-search RET part RET`. The verb
_part_ contains 12 senses. Its Synonyms/Hypernyms section has an
entry:

~~~
** Sense 3
depart, part, start, start out, set forth, set off, set out, take off
	   => leave, go forth, go away
		  Phrasal Verb-> part with#1
~~~

To which meaning of all senses it exactly corresponds? You could
scroll back to the overview section, manually find the _verb_ section
& look into the item 3. Then you scroll back.

Instead of doing this, press <kbd>o</kbd> when the cursor is somewhere
in the `Sense 3` subsection to auto-retrieve the text from the
overview. If you press <kbd>o</kbd> (or <kbd>Enter</kbd>) when the
curson is inside `part with#1`word, it gets you to the wordnet entry
_part with_ → _verb_ → _sense 1_. To return to the _part_ word, press
<kbd>l</kbd>.

## Bugs

* ≈ 18KB is too much for such a small major mode.

## TODO

* Display a lexical category in the echo area akin to eldoc.
* Custom URI handler for `wordnut:part%20with#verb/1`.
* Mouse support.

## Credits

The inspiration was [wn-org.el](http://emacswiki.org/emacs/wn-org.el)
mode.

## License

GPLv2.
