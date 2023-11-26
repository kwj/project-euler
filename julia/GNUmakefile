
SUBDIRS = Euler

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove backup files'

.PHONY: clean
clean:
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: list
list:
	@ls Euler/src/bin/p*.jl | sed -e 's/.jl.*//' -e 's#.*/##' -e 's/^p0*//'
