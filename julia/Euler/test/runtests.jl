
using Euler
using Test

# read test cases for solutions
include.(filter(contains(r"tc[0-9]+.jl$"), readdir("./")))
