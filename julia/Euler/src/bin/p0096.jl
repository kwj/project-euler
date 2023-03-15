
# project euler: problem 96

module Prob0096

import ..AlgoX: dlx_init, dlx_add_row, dlx_solve

#=
  [Grid]
      C1 C2 C3 C4 C5 C6 C7 C8 C9
     +--------+--------+--------+    R: Row
   R1|        |        |        |    C: Column
   R2|   B1   |   B2   |   B3   |    B: Box
   R3|        |        |        |
     +--------+--------+--------+    Cell(x,y) = RxCy
   R4|        |        |        |      position = 9x + y
   R5|   B4   |   B5   |   B6   |
   R6|        |        |        |
     +--------+--------+--------+
   R7|        |        |        |
   R8|   B7   |   B8   |   B9   |
   R9|        |        |        |
     +--------+--------+--------+

  [Matrix]
         R1C1 R1C2 .. R9C9 | R1#1 R1#2 .. R9#8 R9#9 | C1#1 C1#2 .. C9#8 C9#9 | B1#1 B1#2 .. B9#8 B9#9
  ----------------------------------------------------------------------------------------------------
  R1C1#1   1    0  ..   0      1    0  ..   0    0      1    0  ..   0    0      1    0  ..   0    0
  R1C1#2   1    0  ..   0      0    1  ..   0    0      0    1  ..   0    0      0    1  ..   0    0
   ...
  R1C1#9   1    0  ..   0      0    0  ..   0    0      0    0  ..   0    0      0    0  ..   0    0
  R1C2#1   0    1  ..   0      1    0  ..   0    0      0    0  ..   0    0      1    0  ..   0    0
   ...
  R1C9#9   0    0  ..   0      0    0  ..   0    0      0    0  ..   0    1      0    0  ..   0    0
  R2C1#1   0    0  ..   0      0    0  ..   0    0      1    0  ..   0    0      0    0  ..   0    0
   ...
  R9C9#8   0    0  ..   1      0    0  ..   1    0      0    0  ..   1    0      0    0  ..   1    0
  R9C9#9   0    0  ..   1      0    0  ..   0    1      0    0  ..   0    1      0    0  ..   0    1

    Row: RxCy#N -> Cell(x,y) = N
    Col: RxCy -> some number is in Cell(x,y)       [cell constraint] (81 columns)
         Rx#N -> number 'N' is in the row Rx       [row constraint] (81 columns)
         Cy#N -> number 'N' is in the column Cy    [column constraint] (81 columns)
         Bz#N -> number 'N' is in the box Bz       [box constraint] (81 columns)

=#

function make_dlx(q)
    function add_row(pos, n)
        r, c, b = cld(pos, 9), mod1(pos, 9), (cld(pos, 27) - 1) * 3 + cld(mod1(pos, 9), 3)
        function aux(num::Int)
            tag = "R" * string(r) * "C" * string(c) * "#" * string(num)
            row_data = [pos, 81 + (r - 1) * 9 + num, 81 * 2 + (c - 1) * 9 + num, 81 * 3 + (b - 1) * 9 + num]
            dlx_add_row(d, row_data, tag)
        end

        if n != 0
            aux(n)
        else
            for i = 1:9
                aux(i)
            end
        end
    end

    # 324 is the number of columns. (81 + 81 + 81 + 81)
    d = dlx_init(324)
    for (i, n) in enumerate(map((x) -> parse(Int, x), split(q, "")))
        add_row(i, n)
    end
    d
end

function parse_data(fname)
    trim(lst::Vector{String}) = replace(reduce(*, lst), r"[^0-9.]" => "", "." => "0")

    result = Array{String}(undef, 0)
    acc = Array{String}(undef, 0)
    for line in readlines(joinpath((@__DIR__), "../../assets", fname))
        if match(r"^[0-9.]", line) !== nothing
            push!(acc, line)
        elseif match(r"^-", line) !== nothing
            continue
        elseif length(acc) != 0
            push!(result, trim(acc))
            empty!(acc)
        end
    end
    if length(acc) != 0
        push!(result, trim(acc))
    end

    for data in result
        if length(data) != 81
            println("[Warning] input data is invalid (ignored): ", data)
        end
    end
    filter((x) -> length(x) == 81, result)
end

function solve_0096(fname::String = "p096_sudoku.txt")
    pickup_num(s::Vector{String}) = foldl((acc, i) -> 10 * acc + i, map((x) -> parse(Int, x[end]), s))

    answer = 0
    for q in parse_data(fname)
        d = make_dlx(q)
        result = dlx_solve(d)
        if length(result) == 1
            answer += pickup_num(sort(result[1])[1:3])
        else
            println("[Warning] zero or more than one solutions: ", q)
        end
    end
    answer
end

end #module

using .Prob0096: solve_0096
export solve_0096
