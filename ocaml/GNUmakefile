
SUBDIRS = bin src lib

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove backup files'
	@echo '  make clean-all'
	@echo '    remove build files and backup files'

.PHONY: all
all:
	@dune build

.PHONY: test
test:
	@dune runtest

.PHONY: clean
clean:
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: clean-all
clean-all:
	@dune clean
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: list
list:
	@ls _build/default/bin/p*.*exe | sed -e 's#.*/##' -e 's/.exe.*//' -e 's/^p0*//'
