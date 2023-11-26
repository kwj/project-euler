
SUBDIRS = src

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make build'
	@echo '    compile all programs (with the release profile)'
	@echo '  make test'
	@echo '    run all regression tests (with the release profile)'
	@echo '  make clean'
	@echo '    remove backup files'
	@echo '  make clean-all'
	@echo '    remove the target directory and backup files'

.PHONY: build
build:
	@cargo build --release --package euler

.PHONY: test
test:
	@cargo test --release --package euler tests

.PHONY: lint
lint:
	@cargo clippy

.PHONY: clean
clean:
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: clean-all
clean-all:
	@cargo clean
	@find . -name '*~' -o -name '#*' | xargs -r rm
