
PROBLEMS  = p0001 p0002 p0003 p0004 p0005 p0006 p0007 p0008 p0009 p0010
PROBLEMS += p0011

p0008_PKGS = str

%: %.ml
	ocamlfind ocamlopt -o cmd/$@ $(if $($@_PKGS),-linkpkg $(patsubst %,-package %,$($@_PKGS))) $<

.PHONY: usage
usage:
	@echo 'Usage: make <problem>'
	@echo '  problem: p0001, p0002, ...'

.PHONY: all
all: $(PROBLEMS)

.PHONY: clean
clean:
	rm -f cmd/* *.o *.cmi *.cmx *~
