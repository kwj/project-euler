
# project euler: problem 54

#=
  We'll need the following file to run this program.
    - https://projecteuler.net/project/resources/p054_poker.txt

  card rank:
    2, 3, 4, 5, 6, 7, 8, 9, Ten(10), Jack(11), Queen(12), King(13), Ace(14)

  hand:
    0 - High Card: Highest value card.
    1 - One Pair: Two cards of the same value.
    2 - Two Pairs: Two different pairs.
    3 - Three of a Kind: Three cards of the same value.
    4 - Straight: All cards are consecutive values.
    5 - Flush: All cards of the same suit.
    6 - Full House: Three of a kind and a pair.
    7 - Four of a Kind: Four cards of the same value.
    8 - Straight Flush: All cards are consecutive values of same suit.
    9 - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.

  hand rank:
    [hand, val_1, val_2, ...]  val_# : rank detail
      example:
        8H 3D JS 6S 4C -> [0, 11, 8, 6, 4, 3]      - HC: High Card [0; 11] @ kicker: [8; 6; 4; 3]
        9S 3C 9C 5S JS -> [1, 9, 11, 5, 3]         - OP: One Pair [1; 9] @ kicker : [11; 5; 3]
        5C AD 5D AC 9C -> [2, 14, 5; 9]            - TP: Two Pair [2; 14; 5] @ kicker : [9]
        3H 8S 7D 7H 7S -> [3, 7, 8, 3]             - TK: Three of a Kind [3; 7] @ kicker : [8; 3]
        7H 5D 6S 8H 9H -> [4, 9, 8, 7, 6, 5]       - S:  Straight [4; 9; 8; 7; 6; 5]
        2H 6H 7H QH JH -> [5, 12, 11, 7, 6, 2]     - F:  Flush [5; 12; 11; 7; 6; 2]
        4D 8C 8S 4S 4H -> [6, 4, 8]                - FH: Full House [6; 4; 8]
        3S 8H 3D 3H 3C -> [7, 3, 8]                - FK: Four of a Kind [7; 3] @ kicker : [8]
        8C 6C 7C 5C 9C -> [8, 9, 8, 7, 6, 5]       - SF: Straight Flush [8; 9; 8; 7; 6; 5]
        AH JH TH QH KH -> [9, 14, 13, 12, 11, 10]  - RF: Royal Flush [9; 14; 13; 12; 11; 10]
=#

module Prob0054

function countmap(tpl::NTuple{5, Int})
    tbl = Dict{Int, Int}()
    for elt in tpl
        haskey(tbl, elt) ? tbl[elt] += 1 : tbl[elt] = 1
    end
    tbl
end

function cmp_pair(x::Pair{Int, Int}, y::Pair{Int, Int})
    if x[2] != y[2]
        return isless(x[2], y[2])
    end
    return isless(x[1], y[1])
end

function make_handinfo(cards::Vector{Tuple{Int, Char}})
    HAND_RF, HAND_SF,HAND_FK, HAND_FH, HAND_F = [9], [8], [7], [6], [5]
    HAND_S, HAND_TK, HAND_TP, HAND_OP, HAND_HC = [4], [3], [2], [1], [0]
    is_straight(lst) = lst == lst[1]:-1:(lst[1] - 4)

    nums, suits = collect(zip(cards...))
    flash = length(unique(suits)) == 1
    hand_n, hand_c = collect.(collect(zip(sort(collect(countmap(nums)), lt=cmp_pair, rev=true)...)))

    if flash == true
        if is_straight(hand_n) == true
            if hand_c[1] == 14
                return vcat(HAND_RF, hand_n)
            else
                return vcat(HAND_SF, hand_n)
            end
        else
            return vcat(HAND_F, hand_n)
        end
    else
        num_of_hand_n = length(hand_n)
        if num_of_hand_n == 5
            if is_straight(hand_n) == true
                return vcat(HAND_S, hand_n)
            else
                return vcat(HAND_HC, hand_n)
            end
        elseif num_of_hand_n == 4
            return vcat(HAND_OP, hand_n)
        elseif num_of_hand_n == 3
            if hand_c[1] == 3
                return vcat(HAND_TK, hand_n)
            else
                return vcat(HAND_TP, hand_n)
            end
        elseif num_of_hand_n == 2
            if hand_c[1] == 4
                return vcat(HAND_FK, hand_n)
            else
                return vcat(HAND_FH, hand_n)
            end
        else
            @assert false "not reached"
        end
    end
end

function judge(p1::Vector{Int}, p2::Vector{Int})
    if p1 > p2
        return 1
    elseif p1 < p2
        return -1
    else
        return 0
    end
end

function solve_0054(fname = "p054_poker.txt")
    rank_tbl = Dict([('2', 2), ('3', 3), ('4', 4), ('5', 5), ('6', 6), ('7', 7), ('8', 8),
                     ('9', 9), ('T', 10), ('J', 11), ('Q', 12), ('K', 13), ('A', 14)])

    data = map(split.(readlines(joinpath((@__DIR__), "../../assets", fname)), " ")) do lst
        broadcast(lst) do card
            (rank_tbl[card[1]], card[2])
        end
    end

    p1, p2, draw = 0, 0, 0
    for cards in data
        match = judge(make_handinfo(cards[1:5]), make_handinfo(cards[6:10]))
        if match == 1
            p1 += 1
        elseif match == -1
            p2 += 1
        else
            draw += 1
        end
    end
    p1
end

end #module

using .Prob0054: solve_0054
export solve_0054
