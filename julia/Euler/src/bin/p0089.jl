
# project euler: problem 89

#=
  step 1:
    IIIIIIIII     IX
    XXXXXXXXX     XC
    CCCCCCCCC     CM

  step 2:
    VIIII         IX
    LXXXX         XC
    DCCCC         CM

  step 3:
    IIIII         V
    XXXXX         L
    CCCCC         D

  step 4:
    IIII          IV
    XXXX          XL
    CCCC          CD
=#

module Prob0089

function replace_numbers(line)
    replace(line, r"IIIIIIIII|XXXXXXXXX|CCCCCCCCC" => "##",
                  r"VIIII|LXXXX|DCCCC" => "##",
                  r"IIIII|XXXXX|CCCCC" => "#",
                  r"IIII|XXXX|CCCC" => "##")
end

function solve_0089(fname = "0089_roman.txt")
    data = readlines(joinpath((@__DIR__), "../../assets", fname))

    acc = 0
    for line in data
        acc += length(line) - length(replace_numbers(line))
    end
    acc
end

end #module

using .Prob0089: solve_0089
export solve_0089
