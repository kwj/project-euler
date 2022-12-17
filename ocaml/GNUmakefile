
.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove build files and backup files'
	@echo '  make clean-backups'
	@echo '    remove backup files only'

.PHONY: all
all:
	@dune build

.PHONY: clean
clean:
	@dune clean
	@rm -f bin/*~ lib/*~ ./*~

.PHONY: clean-backups
clean-backups:
	@rm -f bin/*~ lib/*~ ./*~

.PHONY: list
list:
	@ls _build/default/bin/p*.*exe | sed -e 's#.*/##' -e 's/.exe.*//'
