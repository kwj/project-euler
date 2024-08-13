
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
	@go test ./internal/sol/...

.PHONY: all-test
all-test:
	@go clean -testcache
	@go test ./internal/sol/...

.PHONY: lint
lint:
	@go vet .

.PHONY: clean
clean:
	@rm -f ./pe-solver
	@find . -name '*~' -o -name '#*' | xargs -r rm
