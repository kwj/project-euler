module Sol.P0084 (compute, solve) where

-- Note:
-- I feel that I just happened to get the right answer.

import Data.Array.ST (modifyArray, newArray, runSTUArray)
import Data.Array.Unboxed (UArray, bounds, elems, listArray, (!))
import Data.Foldable (for_)
import Data.Function (on)
import Data.Ix (range)
import Data.List (sortBy)
import Text.Printf (printf)

import Mylib.Util (partitionByStep)

type Pos = (Int, Int)
type Percentage = Double
type PercentArray = UArray Pos Percentage

{- FOURMOLU_DISABLE -}
data Squares
    = GO | A1 | CC1 | A2 | T1 | R1 | B1 | CH1 | B2 | B3
    | JAIL | C1 | U1 | C2 | C3 | R2 | D1 | CC2 | D2 | D3
    | FP | E1 | CH2 | E2 | E3 | R3 | F1 | F2 | U2 | F3
    | G2J | G1 | G2 | CC3 | G3 | R4 | CH3 | H1 | T2 | H2
    deriving (Show, Enum, Eq)
{- FOURMOLU_ENABLE -}

nSquares :: Int
nSquares = 40

nextStates :: Pos -> Percentage -> Int -> [(Pos, Percentage)]
nextStates _ 0 _ = []
nextStates (sq, dbl) pct nfaces =
    chanceCard
        =<< ( communityChest
                . (\(st, p) -> if toEnum (fst st) /= G2J then (st, p) else ((fromEnum JAIL, 0), p))
                . (\(st, p) -> if snd st /= 3 then (st, p) else ((fromEnum JAIL, 0), p))
                =<< next_state
            )
  where
    next_state =
        [ ((next_sq, next_dbl), pct / fromIntegral (nfaces * nfaces))
        | d1 <- [1 .. nfaces]
        , d2 <- [1 .. nfaces]
        , let next_sq = (sq + d1 + d2) `mod` nSquares
        , let next_dbl = if d1 == d2 then dbl + 1 else 0
        ]

{- FOURMOLU_DISABLE -}
    chanceCard :: (Pos, Percentage) -> [(Pos, Percentage)]
    chanceCard (next_pos, next_pct)
        | square /= CH1 && square /= CH2 && square /= CH3 =
            [(next_pos, next_pct)]
        | otherwise =
            let cards =
                    [ fromEnum GO, fromEnum JAIL, fromEnum C1, fromEnum E3
                    , fromEnum H2, fromEnum R1, fromEnum (nextR square), fromEnum (nextR square)
                    , fromEnum (nextU square), (fst next_pos + (nSquares - 3)) `mod` nSquares, fst next_pos, fst next_pos
                    , fst next_pos, fst next_pos, fst next_pos, fst next_pos
                    ]
             in (\c -> ((c, snd next_pos), next_pct / fromIntegral (length cards))) <$> cards
      where
        square = toEnum (fst next_pos) :: Squares

        nextR :: Squares -> Squares
        nextR CH1 = R2
        nextR CH2 = R3
        nextR CH3 = R1
        nextR _ = error "fatal error"

        nextU :: Squares -> Squares
        nextU CH1 = U1
        nextU CH2 = U2
        nextU CH3 = U1
        nextU _ = error "fatal error"

    communityChest :: (Pos, Percentage) -> [(Pos, Percentage)]
    communityChest (next_pos, next_pct)
        | square /= CC1 && square /= CC2 && square /= CC3 =
            [(next_pos, next_pct)]
        | otherwise =
            let cards =
                    [ fromEnum GO, fromEnum JAIL, fst next_pos, fst next_pos
                    , fst next_pos, fst next_pos, fst next_pos, fst next_pos
                    , fst next_pos, fst next_pos, fst next_pos, fst next_pos
                    , fst next_pos, fst next_pos, fst next_pos, fst next_pos
                    ]
             in (\c -> ((c, snd next_pos), next_pct / fromIntegral (length cards))) <$> cards
      where
        square = toEnum (fst next_pos) :: Squares
{- FOURMOLU_ENABLE -}

nextPctArray :: PercentArray -> Int -> PercentArray
nextPctArray arr nfaces =
    runSTUArray $ do
        result <- newArray (bounds arr) 0
        for_ (range (bounds arr)) $ \idx ->
            for_ (nextStates idx (arr ! idx) nfaces) $ \(pos, pct) ->
                modifyArray result pos (+ pct)
        pure result

makeSteadyState :: PercentArray -> Int -> [(Percentage, Int)]
makeSteadyState pct_arr nfaces =
    zip
        (map sum . partitionByStep 3 3 . elems $ aux pct_arr 1000) -- number of iterations with no evidence
        [0 ..]
  where
    aux :: PercentArray -> Int -> PercentArray
    aux arr 0 = arr
    aux arr cnt = aux (nextPctArray arr nfaces) (pred cnt)

compute :: Int -> String
compute nfaces =
    concatMap (printf "%02d" . snd)
        . take 3
        . sortBy (flip compare `on` fst)
        $ makeSteadyState
            (listArray ((0, 0), (nSquares - 1, 2)) (100.0 : repeat 0))
            nfaces

solve :: String
solve = compute 4
