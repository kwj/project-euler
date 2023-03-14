
module Euler

# read libraries
include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "lib"), join=true)))

# read solutions
include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "bin"), join=true)))

end # module Euler
