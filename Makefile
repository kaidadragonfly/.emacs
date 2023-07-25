.PHONY: all

all: load.elc init.elc ide.elc

load.el: $(wildcard load/*.el)
	cat load/*.el > load.el

install-packages.elc:
	emacs --batch --script install-packages.el

%.elc: %.el
	emacs --batch --eval '(progn (package-initialize)(byte-compile-file "'$<'"))'
