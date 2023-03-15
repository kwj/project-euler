
# project euler: problem 84

#=
  Julia has the standard library of linear algebra.
  I therefore solved this problem using Markov chain and the library.

  Stochastic matrix: stoch_matrix
         | GO  A1  CC1 ... [current square]
    -----+---------------------------
     GO  | ##  ##  ##  ...
     A1  | ##  ##  ##  ...
     CC1 | ##  ##  ##  ...
         .  .   .   .
  [next square]
=#

module Prob0084

import LinearAlgebra: nullspace, I

function solve_0084(nfaces::Int = 4, nsquares::Int = 3)
    (GO,   A1, CC1, A2,  T1, R1, B1,  CH1, B2, B3,
     JAIL, C1, U1,  C2,  C3, R2, D1,  CC2, D2, D3,
     FP,   E1, CH2, E2,  E3, R3, F1,  F2,  U2, F3,
     G2J,  G1, G2,  CC3, G3, R4, CH3, H1,  T2, H2) = collect(1:40)

    dice_prblty = zeros(nfaces * 2)
    for n1 = 1:nfaces, n2 = 1:nfaces
        dice_prblty[n1 + n2] += 1 / (nfaces ^ 2)
    end

    stoch_matrix = zeros(40, 40)
    for r = 1:40, v = 2:(nfaces * 2)
        stoch_matrix[mod1(r + v, 40), r] = dice_prblty[v]
    end

    # Go to Jail
    stoch_matrix[JAIL, :] += stoch_matrix[G2J, :]
    stoch_matrix[G2J, :] .= 0

    # Chance Card
    # note: It must be processed before Communy Chest because the CH3 -> CC3 path is exist.
    for chance in [CH1, CH2, CH3]
        for (current, prblty) = pairs(stoch_matrix[chance, :])
            next_R = (chance == CH1) ? R2 : ((chance == CH2) ? R3 : R1)
            next_U = (chance == CH2) ? U2 : U1
            for next_sq in (GO, JAIL, C1, E3, H2, R1, next_R, next_R, next_U, mod1(chance - 3, 40))
                stoch_matrix[next_sq, current] += prblty / 16
            end
            stoch_matrix[chance, current] -= (prblty / 16) * 10
        end
    end

    # Community Chest
    for chest in [CC1, CC2, CC3]
        stoch_matrix[GO, :] += stoch_matrix[chest, :] ./ 16
        stoch_matrix[JAIL, :] += stoch_matrix[chest, :] ./ 16
        stoch_matrix[chest, :] -= stoch_matrix[chest, :] ./ 8
    end

    null_space = nullspace(stoch_matrix - I)
    # note: If we want the stationary distribution of 'stoch_matrix', we must normalized 'null_space'.
    # null_space ./= sum(null_space)

    # Return the answer as string because it must be a (nsquares * 2) digits.
    answer = ""
    for sq in sort(collect(0:39); by = (x) -> null_space[x + 1], rev = true)[1:nsquares]
        answer *= lpad(string(sq), 2, "0")
    end
    answer
end

end #module

using .Prob0084: solve_0084
export solve_0084
