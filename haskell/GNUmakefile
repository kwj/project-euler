
.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make build'
	@echo '    compile all programs (with the release profile)'
	@echo '  make test'
	@echo '    run all regression tests to solutions
	@echo '  make clean'
	@echo '    remove backup files'

.PHONY: build
build:
	@cabal build

.PHONY: test
test:
	@cabal test

.PHONY: clean
clean:
	@cabal clean
	@find . -name '*~' -o -name '#*' | xargs -r rm
