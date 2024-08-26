
SUBDIRS = euler

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make test'
	@echo '    run all regression tests'
	@echo '  make clean'
	@echo '    remove backup files'

.PHONY: test
test:
	@deno task --config ./euler/deno.json test

.PHONY: clean
clean:
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: list
list:
	@ls euler/bin/p*.ts | sed -e 's/.ts.*//' -e 's#.*/##' -e 's/^p0*//'

