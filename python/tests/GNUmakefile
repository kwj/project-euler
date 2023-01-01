
SUBDIRS = bin

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make clean'
	@echo '    remove backup files only'
	@echo '  make clean-all'
	@echo '    remove cache files and backup files'

.PHONY: clean
clean:
	@rm -f ./*~
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean; \
	done

.PHONY: clean-all
clean-all:
	@rm -f ./*~ ./__pycache__/*
	@for subdir in $(SUBDIRS); do \
	  make -C $$subdir clean-all; \
	done
