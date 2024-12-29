elc := $(patsubst %.el, %.elc, $(wildcard *.el))

lint: $(elc)
	rm $^
	stat -c %s *.el | awk '{s += $$1} END {print s}'

%.elc: %.el
	emacs -Q -batch -L . -f batch-byte-compile $<
