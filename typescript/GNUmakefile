
SUBDIRS = euler

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make test'
	@echo '    run all regression tests'
	@echo '  make clean'
	@echo '    remove backup files only'

.PHONY: test
test:
	@deno task --config ./euler/deno.json test

.PHONY: clean
clean:
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: list
list:
	@ls euler/bin/p*.ts | sed -e 's/.ts.*//' -e 's#.*/##' -e 's/^p0*//'

