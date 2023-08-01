
# test cases for problems (#1 .. #100)

@testset verbose = true "0001 - 0100" begin
    @testset "0001" begin
        @test solve_0001(10) == 23
        @test solve_0001(1_000) == 233168
    end

    @testset "0002" begin
        @test solve_0002(100) == 44
        @test solve_0002(4_000_000) == 4613732
    end

    @testset "0003" begin
        @test solve_0003(13195) == 29
        @test solve_0003(600851475143) == 6857
    end

    @testset "0004" begin
        @test solve_0004(2) == 9009
        @test solve_0004(3) == 906609
    end

    @testset "0005" begin
        @test solve_0005(1, 10) == 2520
        @test solve_0005(1, 20) == 232792560
    end

    @testset "0006" begin
        @test solve_0006(1, 10) == 2640
        @test solve_0006(1, 100) == 25164150
    end

    @testset "0007" begin
        @test solve_0007(6) == 13
        @test solve_0007(10_001) == 104743
    end

    @testset "0008" begin
        @test solve_0008(4) == 5832
        @test solve_0008(13) == 23514624000
    end

    @testset "0009" begin
        @test solve_0009(3+4+5) == 3 * 4 * 5
        @test solve_0009(9+12+15) == 9 * 12 * 15
        @test solve_0009(1_000) == 31875000
    end

    @testset "0010" begin
        @test solve_0010(10) == 17
        @test solve_0010(2_000_000) == 142913828922
    end

    @testset "0011" begin
        @test solve_0011(4) == 70600674
    end

    @testset "0012" begin
        @test solve_0012(5) == 28
        @test solve_0012(500) == 76576500
    end

    @testset "0013" begin
        @test solve_0013(10) == 5537376230
    end

    @testset "0014" begin
        @test solve_0014(1_000_000) == 837799
    end

    @testset "0015" begin
        @test solve_0015(2, 2) == 6
        @test solve_0015(20, 20) == 137846528820
    end

    @testset "0016" begin
        @test solve_0016(15) == 26
        @test solve_0016(1_000) == 1366
    end

    @testset "0017" begin
        @test solve_0017(5) == 19
        @test solve_0017(1_000) == 21124
    end

    @testset "0018" begin
        @test solve_0018(max) == 1074
    end

    @testset "0019" begin
        @test solve_0019() == 171
    end

    @testset "0020" begin
        @test solve_0020(10) == 27
        @test solve_0020(100) == 648
    end

    @testset "0021" begin
        @test solve_0021(10_000) == 31626
    end

    @testset "0022" begin
        @test solve_0022("p022_names.txt") == 871198282
    end

    @testset "0023" begin
        @test solve_0023(28_123) == 4179871
    end

    @testset "0024" begin
        @test solve_0024(1_000_000, collect(0:9)) == 2783915460
    end

    @testset "0025" begin
        @test solve_0025(3) == 12
        @test solve_0025(1_000) == 4782
    end

    @testset "0026" begin
        @test solve_0026(10) == 7
        @test solve_0026(300) == 289
        @test solve_0026(1_000) == 983
    end

    @testset "0027" begin
        @test solve_0027() == -59231
    end

    @testset "0028" begin
        @test solve_0028(5) == 101
        @test solve_0028(1_001) == 669171001
    end

    @testset "0029" begin
        @test solve_0029(100) == 9183
    end

    @testset "0030" begin
        @test solve_0030(4) == 19316
        @test solve_0030(5) == 443839
    end

    @testset "0031" begin
        @test solve_0031([1, 2, 5, 10, 20, 50, 100, 200], 200) == 73682
    end

    @testset "0032" begin
        @test solve_0032() == 45228
    end

    @testset "0033" begin
        @test solve_0033() == 100
    end

    @testset "0034" begin
        @test solve_0034() == 40730
    end

    @testset "0035" begin
        @test solve_0035(100) == 13
        @test solve_0035(1_000_000) == 55
    end

    @testset "0036" begin
        @test solve_0036(1_000_000) == 872187
    end

    @testset "0037" begin
        @test solve_0037() == 748317
    end

    @testset "0038" begin
        @test solve_0038() == 932718654
    end

    @testset "0039" begin
        @test solve_0039(1_000) == 840
    end

    @testset "0040" begin
        @test solve_0040() == 210
    end

    @testset "0041" begin
        @test solve_0041() == 7652413
    end

    @testset "0042" begin
        @test solve_0042("p042_words.txt") == 162
    end

    @testset "0043" begin
        @test solve_0043() == 16695334890
    end

    @testset "0044" begin
        @test solve_0044() == 5482660
    end

    @testset "0045" begin
        @test solve_0045() == 1533776805
    end

    @testset "0046" begin
        @test solve_0046() == 5777
    end

    @testset "0047" begin
        @test solve_0047(2) == 14
        @test solve_0047(3) == 644
        @test solve_0047(4) == 134043
    end

    @testset "0048" begin
        @test solve_0048(10) == "0405071317"
        @test solve_0048(1_000) == "9110846700"
    end

    @testset "0049" begin
        @test solve_0049() == 296962999629
    end

    @testset "0050" begin
        @test solve_0050(100) == 41
        @test solve_0050(1_000) == 953
        @test solve_0050(1_000_000) == 997651
    end

    @testset "0051" begin
        @test solve_0051() == 121313
    end

    @testset "0052" begin
        @test solve_0052() == 142857
    end

    @testset "0053" begin
        @test solve_0053(23, 1_000_000) == 4
        @test solve_0053(100, 1_000_000) == 4075
    end

    @testset "0054" begin
        @test solve_0054("p054_poker.txt") == 376
    end

    @testset "0055" begin
        @test solve_0055(10_000) == 249
    end

    @testset "0056" begin
        @test solve_0056() == 972
    end

    @testset "0057" begin
        @test solve_0057(8) == 1
        @test solve_0057(1_000) == 153
    end

    @testset "0058" begin
        @test solve_0058() == 26241
    end

    @testset "0059" begin
        @test solve_0059("p059_cipher.txt") == 129448
    end

    @testset "0060" begin
        @test solve_0060(4) == 792
        @test solve_0060(5) == 26033
    end

    @testset "0061" begin
        @test solve_0061() == 28684
    end

    @testset "0062" begin
        @test solve_0062(3) == 41063625
        @test solve_0062(5) == 127035954683
    end

    @testset "0063" begin
        @test solve_0063() == 49
    end

    @testset "0064" begin
        @test solve_0064(13) == 4
        @test solve_0064(10_000) == 1322
    end

    @testset "0065" begin
        @test solve_0065(10) == 17
        @test solve_0065(100) == 272
    end

    @testset "0066" begin
        @test solve_0066(7) == 5
        @test solve_0066(1_000) == 661
    end

    @testset "0067" begin
        @test solve_0067("p067_triangle.txt", max) == 7273
    end

    @testset "0068" begin
        @test solve_0068(3) == 432621513
        @test solve_0068(5) == 6531031914842725
    end

    @testset "0069" begin
        @test solve_0069(10) == 6
        @test solve_0069(1_000_000) == 510510
    end

    @testset "0070" begin
        @test solve_0070() == 8319823
    end

    @testset "0071" begin
        @test solve_0071(8) == 2
        @test solve_0071(1_000_000) == 428570
    end

    @testset "0072" begin
        @test solve_0072(8) == 21
        @test solve_0072(1_000_000) == 303963552391
    end

    @testset "0073" begin
        @test solve_0073(8) == 3
        @test solve_0073(12_000) == 7295372
    end

    @testset "0074" begin
        @test solve_0074(1_000_000, 60) == 402
    end

    @testset "0075" begin
        @test solve_0075(1_500_000) == 161667
    end

    @testset "0076" begin
        @test solve_0076(collect(1:99), 100) == 190569291
    end

    @testset "0077" begin
        @test solve_0077(4) == 10
        @test solve_0077(5_000) == 71
    end

    @testset "0078" begin
        @test solve_0078(1_000_000) == 55374
    end

    @testset "0079" begin
        @test solve_0079("p079_keylog.txt") == 73162890
    end

    @testset "0080" begin
        @test solve_0080(2, 100) == 475
        @test solve_0080(100, 100) == 40886
    end

    @testset "0081" begin
        @test solve_0081("p081_matrix.txt", min) == 427337
    end

    @testset "0082" begin
        @test solve_0082("p082_matrix.txt", min) == 260324
    end

    @testset "0083" begin
        @test solve_0083("p083_matrix.txt") == 425185
    end

    @testset "0084" begin
        @test solve_0084(6, 3) == "102400"
        @test solve_0084(4, 3) == "101524"
    end

    @testset "0085" begin
        @test solve_0085(2_000_000) == 2772
    end

    @testset "0086" begin
        @test solve_0086(1_975) == 99
        @test solve_0086(2_060) == 100
        @test solve_0086(1_000_000) == 1818
    end

    @testset "0087" begin
        @test solve_0087(50) == 4
        @test solve_0087(50_000_000) == 1097343
    end

    @testset "0088" begin
        @test solve_0088(6) == 30
        @test solve_0088(12) == 61
        @test solve_0088(12_000) == 7587457
    end

    @testset "0089" begin
        @test solve_0089("p089_roman.txt") == 743
    end

    @testset "0090" begin
        @test solve_0090() == 1217
    end

    @testset "0091" begin
        @test solve_0091(2, 2) == 14
        @test solve_0091(50, 50) == 14234
    end

    @testset "0092" begin
        @test solve_0092(10) == 7
        @test solve_0092(10_000_000) == 8581146
    end

    @testset "0093" begin
        @test solve_0093() == 1258
    end

    @testset "0094" begin
        @test solve_0094(1_000_000_000) == 518408346
    end

    @testset "0095" begin
        @test solve_0095(1_000_000) == 14316
    end

    @testset "0096" begin
        @test solve_0096("p096_sudoku.txt") == 24702
    end

    @testset "0097" begin
        @test solve_0097(10) == "8739992577"
    end

    @testset "0098" begin
        @test solve_0098("p098_words.txt") == 18769
    end

    @testset "0099" begin
        @test solve_0099("p099_base_exp.txt") == 709
    end

    @testset "0100" begin
        @test solve_0100(1_000_000_000_000) == 756872327473
    end
end
