
# project euler: problem 98

module Prob0098

function get_squares(tbl, n_digits)
    get!(tbl, n_digits) do
        lst = Array{String}(undef, 0)
        for i = (isqrt(10 ^ (n_digits - 1) - 1) + 1):(isqrt(10 ^ n_digits - 1))
            push!(lst, string(i^2))
        end
        lst
    end
end

function check_anagram(words, squares)
    function aux(w1, w2)
        result = 0
        for sq in squares
            trans = Dict(v => k for (k, v) in Dict(zip(sq, w1)))

            if replace(w1, trans...) != sq
                continue
            end

            tmp = replace(w2, trans...)
            if tmp[1] != 0 && tmp in squares
                result = max(result, parse(Int, tmp), parse(Int, sq))
            end
        end
        result
    end

    result = 0
    for (idx, w1) in pairs(words)
        for w2 in words[idx + 1:end]
            result = max(result, aux(w1, w2))
        end
    end
    result
end

function solve_0098(fname::String = "0098_words.txt")
    word_tbl = Dict{String, Vector{String}}()
    for w in split(replace(readline(joinpath((@__DIR__), "../../assets", fname)), "\"" => ""), ",")
        key = join(sort(collect(w)))
        if haskey(word_tbl, key) == true
            push!(word_tbl[key], w)
        else
            word_tbl[key] = [w]
        end
    end

    answer = 0
    sq_tbl = Dict{Int, Vector{String}}()
    for (key, words) in filter(x -> length(x[2]) > 1, word_tbl)
        answer = max(answer, check_anagram(words, get_squares(sq_tbl, length(key))))
    end
    answer
end

end #module

using .Prob0098: solve_0098
export solve_0098
