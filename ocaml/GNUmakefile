
PROBLEMS  = p0001 p0002 p0003 p0004 p0005 p0006 p0007 p0008 p0009 p0010
PROBLEMS += p0011 p0012 p0013 p0014 p0015 p0016 p0017 p0018 p0019 p0020
PROBLEMS += p0021 p0022 p0023 p0024 p0025 p0026 p0027 p0028 p0029 p0030
PROBLEMS += p0031 p0032 p0033 p0034 p0035 p0036 p0037 p0038 p0039 p0040
PROBLEMS += p0041 p0042 p0043

p0008_PKGS = str
p0013_PKGS = zarith
p0015_PKGS = zarith
p0016_PKGS = zarith
p0020_PKGS = zarith
p0022_PKGS = str
p0025_PKGS = zarith
p0029_PKGS = zarith
p0042_PKGS = str

%: %.ml
	ocamlfind ocamlopt -o $@ $(if $($@_PKGS),-linkpkg $(patsubst %,-package %,$($@_PKGS))) $<

.PHONY: usage
usage:
	@echo 'Usage: make <problem>'
	@echo '  problem: p0001, p0002, ...'

.PHONY: all
all: $(PROBLEMS)

.PHONY: clean
clean:
	@rm -f $(PROBLEMS) *.o *.cmi *.cmx *~
