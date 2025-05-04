
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
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: clean-all
clean-all:
	@find . -name '*~' -o -name '#*' | xargs -r rm
	@find . -path '.*/__pycache__/*.py[co]' | xargs -r rm

.PHONY: lint
lint:
	uvx ruff check .

.PHONY: format
format:
	uvx ruff format --diff .

.PHONY: list
list:
	@ls euler/bin/p*.py | sed -e 's/.py.*//' -e 's#.*/##' -e 's/^p0*//'
