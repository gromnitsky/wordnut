.DELETE_ON_ERROR:

elc := $(patsubst %.el, %.elc, $(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

.PHONY: compile
compile: $(elc)

.PHONY: clean
clean:
	rm -f $(elc)
