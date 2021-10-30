
PROBLEMS = p0001 p0002 p0003 p0004 p0005 p0006 p0007 p0008

%: %.ml
	ocamlopt -o cmd/$@ $(wordlist 2, $(words $^), $^) $<

usage:
	@echo 'Usage: make <problem>'
	@echo '  problem: p0001, p0002, ...'

all: $(PROBLEMS)

clean:
	rm -f cmd/* *.o *.cmi *.cmx *~

str.cmxa:

p0001:
p0002:
p0003:
p0004:
p0005:
p0006:
p0007:
p0008: str.cmxa
p0009:
