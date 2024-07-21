module Main where

import Control.DeepSeq (force)
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

import qualified Sol (isExist, solve)

runSolver :: String -> IO ()
runSolver problem =
    if Sol.isExist problem
        then do
            printf "[Problem %s]\n" problem
            start <- getMonotonicTimeNSec
            let result = force (Sol.solve problem)
            end <- getMonotonicTimeNSec
            printf "Answer: %s\n" (fromMaybe "" result)
            printf
                "Elapsed time: %.6f sec.\n\n"
                (fromIntegral (end - start) / 1e9 :: Double)
        else
            printf "No solver exists for problem '%s'.\n\n" problem

solveProblems :: [String] -> IO ()
solveProblems [] = return ()
solveProblems (x : xs) = do
    runSolver x
    solveProblems xs

usage :: IO ()
usage = do
    prog_name <- getProgName
    printf "usage: %s <problem_number ...>\n\n" prog_name

main :: IO ()
main = do
    problems <- getArgs
    case length problems of
        0 -> usage
        _ -> solveProblems problems
