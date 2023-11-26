
SUBDIRS = src/project_euler test/project_euler dev

.PHONY: usage
usage:
	@echo 'Usage:'
	@echo '  make build'
	@echo '    create a Uber-JAR file
	@echo '  make test'
	@echo '    run all tests
	@echo '  make clean'
	@echo '    remove backup files'
	@echo '  make clean-all'
	@echo '    remove the target directory and backup files'

.PHONY: build
build:
	clojure -T:build clean
	clojure -T:build uber

.PHONY: test
test:
	clojure -X:test 

.PHONY: lint
lint:
	clj-kondo --lint $(SUBDIRS)

.PHONY: format
format:
	cljfmt check $(SUBDIRS)

.PHONY: clean
clean:
	@find . -name '*~' -o -name '#*' | xargs -r rm

.PHONY: clean-all
clean-all:
	@find . -name '*~' -o -name '#*' | xargs -r rm
	@rm -rf ./target
