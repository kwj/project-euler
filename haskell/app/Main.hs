module Main where

import Control.DeepSeq (force)
import Data.Foldable (for_)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

import qualified Sol (findSolver)

runSolver :: String -> IO ()
runSolver problem = do
    printf "[Problem %s]\n" problem
    case Sol.findSolver problem of
        Just solver -> do
            start <- getMonotonicTimeNSec
            let result = force solver
            end <- getMonotonicTimeNSec
            printf "Answer: %s\n" result
            printf
                "Elapsed time: %.6f sec.\n\n"
                (fromIntegral (end - start) / 1e9 :: Double)
        Nothing ->
            printf "No solver exists.\n\n"

usage :: IO ()
usage =
    printf "usage: %s <problem_number ...>\n\n" =<< getProgName

main :: IO ()
main = do
    problems <- getArgs
    case length problems of
        0 -> usage
        _ -> for_ problems runSolver
