
# project euler: problem 59

module Prob0059

# valuation rules:
#   0x20 (space): 3
#   0x41 - 0x5A (uppercase letters, 'A' - 'Z'): 5
#   0x61 - 0x7A (lowercase letters, 'a' - 'z'): 3
#   0x21 - 0x7E exclude leters: 1
#   others: 0
function calc_score(c)
    c == 0x20 && return 3
    0x41 <= c <= 0x5A && return 5
    0x61 <= c <= 0x7A && return 2
    0x21 <= c <= 0x7E && return 1
    return 0
end

function solve_0059(fname::String = "p059_cipher.txt")
    cipher_data = [parse(Int, x) for x in split(readline(joinpath((@__DIR__), "../../assets", fname)), ",")]

    score, max_score = 0, 0
    answer = 0
    for key in Iterators.product(Int('a'):Int('z'), Int('a'):Int('z'), Int('a'):Int('z'))
        decrypted_text = [xor(key[mod1(i, 3)], c) for (i, c) in pairs(cipher_data)]
        score = sum(broadcast(calc_score, decrypted_text))
        if score > max_score
            max_score = score
            answer = sum(decrypted_text)
        end
    end
    answer
end

end #module

using .Prob0059: solve_0059
export solve_0059
