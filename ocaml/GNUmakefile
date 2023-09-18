
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

.PHONY: clean
clean:
	@rm -f bin/*~ lib/*~ ./*~

.PHONY: clean-all
clean-all:
	@dune clean
	@rm -f bin/*~ lib/*~ ./*~

.PHONY: list
list:
	@ls _build/default/bin/p*.*exe | sed -e 's#.*/##' -e 's/.exe.*//' -e 's/^p0*//'
