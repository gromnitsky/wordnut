.DELETE_ON_ERROR:

define help =
lint       byte compile *.el
clean
compile    $(pkg)
package    $(name)-$(ver).tar
install    add $(name)-$(ver).tar to $(local-pa)
endef

.PHONY: help
help:
	$(info $(help))@:

pp-%:
	@echo "$(strip $($*))" | tr ' ' \\n

src := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
emacs := 24.4
_meta := $(shell awk -F\" '/wordnut-meta-(name|version)/ {printf("%s ", $$2)}' $(src)/wordnut.el)
name := $(word 1, $(_meta))
ver := $(word 2, $(_meta))
pkg := $(src)/$(name)-pkg.el
desc := $(shell head -1 $(src)/wordnut.el | sed -E 's/^(.+-- )(.+)( -\*-.+)$$/\2/')

elc := $(patsubst %.el, %.elc, $(filter-out $(pkg), $(wildcard $(src)/*.el)))

%.elc: %.el
	emacs -Q -batch -L "$(src)" -f batch-byte-compile $<

.PHONY: lint
lint: $(elc)
	@rm $^
	@cd $(dir $<) && stat -c %s *el | awk '{s += $$1} END {print s}'

.PHONY: clean
clean:
	rm -f $(pkg)

$(name)-$(ver).tar: $(pkg)
	$(if $(filter $(src),$(CURDIR)/),$(error do not run this target in src dir),)
	tar -Pcf $@ $(src)/*.el --transform 's,$(src),$(name)-$(ver),'

.PHONY: package
package: $(name)-$(ver).tar

$(pkg): $(src)/wordnut.el
	echo '(define-package "$(name)" "$(ver)" "$(desc)" (quote ((emacs "$(emacs)"))))' > $@

compile: $(pkg)

local-pa := ~/lib/software/alex/emacs/pkg

.PHONY: install
install: $(name)-$(ver).tar
	emacs -Q -batch -l package-x \
		--eval '(setq package-archive-upload-base "$(local-pa)")' \
		--eval '(package-upload-file "$<")'
# in emacs 24.5, (package-upload-file) corrupts our tar file
	mv $< $(local-pa)
