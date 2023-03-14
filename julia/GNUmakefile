
SUBDIRS = Euler

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove backup files only'

.PHONY: clean
clean:
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: list
list:
	@ls Euler/src/bin/p*.jl | sed -e 's/.jl.*//' -e 's#.*/##' -e 's/^p0*//'
