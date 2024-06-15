
SRCS := .gitignore go.mod main.go solve.sh README.md
PUBLIC_DIR = $(subst project-euler,project-euler-public,$(shell pwd))
SUBDIRS = internal

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
	@go build .

.PHONY: test
test:
	@go clean -testcache
	@go test ./internal/sol/...

.PHONY: lint
lint:
	@go vet .

.PHONY: clean
clean:
	@rm -f ./pe-solver
	@find . -name '*~' -o -name '#*' | xargs -r rm
