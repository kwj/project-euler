
SUBDIRS = euler

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make test'
	@echo '    run all regression tests'
	@echo '  make check'
	@echo '    run type-check'
	@echo '  make lint'
	@echo '    run linter'
	@echo '  make format'
	@echo '    run format check'
	@echo '  make clean'
	@echo '    remove backup files'

.PHONY: test
test:
	@deno task --config ./euler/deno.json test

.PHONY: check
check:
	@deno task --config ./euler/deno.json check

.PHONY: lint
lint:
	@deno task --config ./euler/deno.json lint

.PHONY: format
format:
	@deno task --config ./euler/deno.json format

.PHONY: clean
clean:
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: list
list:
	@ls euler/bin/p*.ts | sed -e 's/.ts.*//' -e 's#.*/##' -e 's/^p0*//'

