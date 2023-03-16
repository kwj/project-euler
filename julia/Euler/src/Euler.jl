
module Euler

# read libraries
include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, "lib"), join=true)))

# read solutions
for bin_dir in filter(x -> match(r"bin.*", x) !== nothing,readdir(@__DIR__))
    include.(filter(contains(r".jl$"), readdir(joinpath(@__DIR__, bin_dir), join=true)))
end

end # module Euler
