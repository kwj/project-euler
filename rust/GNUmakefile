
SUBDIRS = src

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make build'
	@echo '    compile all programs (with the release profile)'
	@echo '  make test'
	@echo '    run all regression tests (with the release profile)'
	@echo '  make clean'
	@echo '    remove backup files only'
	@echo '  make clean-all'
	@echo '    remove the target directory and backup files'

.PHONY: build
build:
	@cargo build --release --package euler

.PHONY: test
test:
	@cargo test --release --package euler tests

.PHONY: clean
clean:
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: clean-all
clean-all:
	@cargo clean
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done
