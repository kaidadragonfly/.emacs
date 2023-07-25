.PHONY: all

all: load.elc init.elc ide.elc

load.el: install-packages.elc $(wildcard load/*.el)
	cat load/*.el > load.el

install-packages.elc: install-packages.el
	emacs --batch --script install-packages.el
	emacs --batch --eval '(progn (package-initialize)(byte-compile-file "'$<'"))'

%.elc: %.el
	emacs --batch --eval '(progn (package-initialize)(byte-compile-file "'$<'"))'
