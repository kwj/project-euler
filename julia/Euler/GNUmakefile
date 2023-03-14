
SUBDIRS = src test

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove backup files only'

.PHONY: clean
clean:
	@rm -f Manifest.toml ./*~
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done
