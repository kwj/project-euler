
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

   Current: S0       S1       S2
        +--------+--------+--------+ State:
  Next  |        |        |        |  S0: no doubles
   S0   |  s00   |  s10   |  s20   |  S1: one doubles occurred
        |        |        |        |  S2: two consecutive dobules occurred
        +--------+--------+--------+
  Next  |        |        |        | s00: transition probability if no doubles occurred (the next state is S0)
   S1   |  s01   | s11=0  | s21=0  | s01: transition probability if fist doubles occurred (the next state is S1)
        |        |        |        |
        +--------+--------+--------+ s10: transition probability if no doubles occurred (the next state is S0)
  Next  |        |        |        | s12: transition probability if two consecutive doubles occurred (the next state is S2)
   S2   | s02=0  |  s12   | s22=0  |
        |        |        |        | s20: transition probability if both doubles and no doubles occurred (the next state is S0)
        +--------+--------+--------+      Note: go to JAIL@S0 if three consecutive doubles occurred
=#

module Prob0084

import LinearAlgebra: nullspace, I

function solve_0084(nfaces::Int = 4, nsquares::Int = 3)
    GO,   A1, CC1, A2,  T1, R1, B1,  CH1, B2, B3,
    JAIL, C1, U1,  C2,  C3, R2, D1,  CC2, D2, D3,
    FP,   E1, CH2, E2,  E3, R3, F1,  F2,  U2, F3,
    G2J,  G1, G2,  CC3, G3, R4, CH3, H1,  T2, H2 = collect(1:40)

    dice_prblty_without_dbl = zeros(nfaces * 2)
    dice_prblty_dbl = zeros(nfaces * 2)
    for n1 = 1:nfaces, n2 = 1:nfaces
        if n1 != n2
            dice_prblty_without_dbl[n1 + n2] += 1 / (nfaces ^ 2)
        else
            dice_prblty_dbl[n1 + n2] = 1 / (nfaces ^ 2)
        end
    end

    stoch_matrix = zeros(40 * 3, 40 * 3)

    # no doubles occurred
    # setup 's00', 's10' and 's20'
    for r = 1:(40 * 3), v = 2:(nfaces * 2)
        stoch_matrix[mod1(r + v, 40), r] = dice_prblty_without_dbl[v]
    end

    # doubles occurrd
    # setup 's01'
    for r = 1:40, v = 2:(nfaces * 2)
        stoch_matrix[mod1(r + v, 40) + 40, r] = dice_prblty_dbl[v]
    end
    # setup 's12'
    for r = 41:80, v = 2:(nfaces * 2)
        stoch_matrix[mod1(r + v, 40) + 80, r] = dice_prblty_dbl[v]
    end
    # modify 's20'
    stoch_matrix[JAIL, 81:120] .+= sum(dice_prblty_dbl)

    # Go to Jail
    for offset = 0:40:80
        stoch_matrix[JAIL, :] += stoch_matrix[G2J + offset, :]
        stoch_matrix[G2J + offset, :] .= 0
    end

    # Chance Card
    # note: It must be processed before Communy Chest because the CH3 -> CC3 path is exist.
    for offset = 0:40:80, chance in [CH1, CH2, CH3]
        for (current, prblty) = pairs(stoch_matrix[chance + offset, :])
            next_R = (chance == CH1) ? R2 : ((chance == CH2) ? R3 : R1)
            next_U = (chance == CH2) ? U2 : U1
            for next_sq in (GO, C1, E3, H2, R1, next_R, next_R, next_U, mod1(chance - 3, 40))
                stoch_matrix[next_sq + offset, current] += prblty / 16
            end
            stoch_matrix[JAIL, current] += prblty / 16

            stoch_matrix[chance, current] -= (prblty / 16) * 10
        end
    end

    # Community Chest
    for offset = 0:40:80, chest in [CC1, CC2, CC3]
        stoch_matrix[GO + offset, :] += stoch_matrix[chest + offset, :] ./ 16
        stoch_matrix[JAIL, :] += stoch_matrix[chest + offset, :] ./ 16
        stoch_matrix[chest + offset, :] -= stoch_matrix[chest + offset, :] ./ 8
    end

    # Ax = λx
    #   A: linear transformation  (in this case, stochastic matrix)
    #   λ: eigenvalue  (in this case, it's one)
    #   x: eigenvector
    # Ax = x ⇔ (A-I)x = 0.  (I is a identity matrix)
    # so, x is a null space of (A - I).
    null_space = nullspace(stoch_matrix - I)

    # Normalize
    null_space ./= sum(null_space)

    # Doubles states are not distinguished.
    # After this, only null_space[1:40] is valid
    for i = 41:120
        null_space[mod1(i, 40)] += null_space[i]
    end

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
