
SRCS := .gitignore solve.py README.md
SUBDIRS = euler tests

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make test'
	@echo '    run all regression tests'
	@echo '  make clean'
	@echo '    remove backup files only'
	@echo '  make clean-all'
	@echo '    remove cache files and backup files'

.PHONY: test
test:
	@python3 -m unittest discover -v --start-directory tests --top-level-directory .

.PHONY: clean
clean:
	@rm -f ./*~
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: clean-all
clean-all:
	@rm -f ./*~
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean-all; \
	done

.PHONY: lint
lint:
	flake8 .
	isort --check --diff .
	black --check --diff .

.PHONY: format
format:
	isort .
	black .

.PHONY: list
list:
	@ls euler/bin/p*.py | sed -e 's/.py.*//' -e 's#.*/##' -e 's/^p0*//'
