
SUBDIRS = bin src lib

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove backup files only'
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
	@rm -f ./*~
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: clean-all
clean-all:
	@dune clean
	@rm -f ./*~
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: list
list:
	@ls _build/default/bin/p*.*exe | sed -e 's#.*/##' -e 's/.exe.*//' -e 's/^p0*//'
