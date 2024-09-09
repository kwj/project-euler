{-# LANGUAGE TemplateHaskell #-}

module Sol.P0054 (compute, solve) where

import Data.List (elemIndex, group, nub, sortBy)
import Data.Maybe (fromJust)
import Data.Ord (Down (..), comparing)

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)

import Mylib.Util (headExn, partitionByStep)

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0054_poker.txt" >>= FE.embedFile)

chToNum :: Char -> Int
chToNum ch =
    case ch of
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        'T' -> 10
        'J' -> 11
        'Q' -> 12
        'K' -> 13
        'A' -> 14
        _ -> error "invalid card"

straightPatterns :: [[Int]]
straightPatterns = partitionByStep 5 1 [14, 13 .. 2]

checkStraight :: [Int] -> Int
checkStraight nums
    | Just v <- elemIndex nums straightPatterns = v
    | otherwise = -1

isFlush :: [Char] -> Bool
isFlush suits = length suits == 1

allHandPatterns :: [[Int]]
allHandPatterns =
    [ [1, 1, 1, 1, 1] -- High Card: 0
    , [2, 1, 1, 1] -- One Pair: 1
    , [2, 2, 1] -- Two Pairs: 2
    , [3, 1, 1] -- Three of a Kind: 3
    , [] -- Straignt: 4 (Note: dummy pattern for elemIndex)
    , [] -- Flush: 5 (Note: dummy pattern for elemIndex)
    , [3, 2] -- Full House: 6
    , [4, 1] -- Four of a Kind: 7
    ]

-- example:
--   ["JH", "2D", "JS", "QD", "AC"] -> [1, 11, 14, 12, 2]
--       One Pair [1, 11] ++ kicker [14, 12, 2]
--   ["5C", "AD", "5D", "AC", "9C"] -> [2, 14, 5, 9]
--       Two Pair [2, 14, 5] ++ kicker [9]
--   ["4D", "8C", "8S", "4S", "4H"] -> [6, 4, 8]
--       Full House [6, 4, 8]
getHand :: [String] -> [Int]
getHand cards
    | isFlush suits =
        let hand = checkStraight nums
         in case hand of
                0 -> 9 : handNums -- Royal Flush: 9
                _
                    | hand > 0 -> 8 : handNums -- Straight Flush: 8
                    | otherwise -> 5 : handNums -- Flush: 5
    | checkStraight nums >= 0 =
        4 : handNums -- Straight: 4
    | otherwise =
        fromJust (elemIndex handPattern allHandPatterns) : handNums
  where
    nums = sortBy (comparing Down) $ chToNum . (!! 0) <$> cards
    suits = nub $ (!! 1) <$> cards
    handPattern = sortBy (comparing Down) $ length <$> group nums
    handNums = headExn <$> sortBy (comparing (Down . length)) $ group nums

parseData :: String -> [([String], [String])]
parseData = map (splitAt 5 . words) . lines

compute :: String
compute =
    show
        . length
        . filter (\(player1, player2) -> getHand player1 > getHand player2) -- Player 1 wins
        $ parseData (BS.unpack fileData)

solve :: String
solve = compute
