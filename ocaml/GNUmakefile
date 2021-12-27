
PROBLEMS  = p0001 p0002 p0003 p0004 p0005 p0006 p0007 p0008 p0009 p0010
PROBLEMS += p0011 p0012 p0013 p0014 p0015 p0016 p0017 p0018 p0019 p0020
PROBLEMS += p0021 p0022 p0023 p0024 p0025 p0026 p0027 p0028 p0029 p0030
PROBLEMS += p0031 p0032 p0033 p0034 p0035 p0036 p0037 p0038 p0039 p0040
PROBLEMS += p0041 p0042 p0043 p0044 p0045 p0046 p0047 p0048 p0049 p0050
PROBLEMS += p0051 p0052 p0053 p0054 p0055 p0056 p0057 p0058 p0059 p0060
PROBLEMS += p0061 p0062 p0063 p0064 p0065 p0066 p0067 p0068 p0069 p0070
PROBLEMS += p0071 p0072 p0073 p0074 p0075 p0076 p0077 p0078 p0079 p0080
PROBLEMS += p0081 p0082 p0083 p0084 p0085 p0086 p0087 p0088 p0089 p0090
PROBLEMS += p0091 p0092 p0093 p0094 p0095 p0096 p0097 p0098 p0099 p0100
PROBLEMS += p0050_alt p0080_alt p0096_alt

p0008_PKGS = str
p0013_PKGS = zarith
p0015_PKGS = zarith
p0016_PKGS = zarith
p0020_PKGS = zarith
p0022_PKGS = str
p0025_PKGS = zarith
p0029_PKGS = zarith
p0042_PKGS = str
p0048_PKGS = zarith
p0052_PKGS = str
p0053_PKGS = zarith
p0054_PKGS = str
p0055_PKGS = zarith
p0056_PKGS = str zarith
p0057_PKGS = zarith
p0059_PKGS = str
p0060_PKGS = zarith
p0062_PKGS = str
p0065_PKGS = zarith
p0066_PKGS = zarith
p0067_PKGS = str
p0069_PKGS = zarith
p0070_PKGS = str
p0077_PKGS = zarith
p0079_PKGS = str
p0080_alt_PKGS = zarith
p0081_PKGS = str
p0082_PKGS = str
p0083_PKGS = str
p0087_PKGS = zarith
p0089_PKGS = str
p0093_PKGS = zarith
p0096_PKGS = str
p0097_PKGS = zarith
p0098_PKGS = str
p0099_PKGS = str
p0096_alt_PKGS = str

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
