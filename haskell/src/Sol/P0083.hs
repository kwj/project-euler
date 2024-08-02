{-# LANGUAGE TemplateHaskell #-}

module Sol.P0083 (compute, solve) where

import Data.Array.IArray (Array, Ix (inRange), bounds, listArray, (!))

import qualified Data.ByteString.Char8 as BS (ByteString, unpack)
import qualified Data.FileEmbed as FE (embedFile, makeRelativeToProject)
import qualified Data.Map.Strict as M (Map, fromList, insert, (!))

import Mylib.Util (headExn)

import qualified Mylib.Heap.LeftistHeap as H (
    LeftistHeap,
    delete,
    empty,
    insert,
    isEmpty,
    peek,
 )

data Cost a = Infinity | Cost a deriving (Show, Eq)

instance Ord a => Ord (Cost a) where
    (<=) :: Ord a => Cost a -> Cost a -> Bool
    Cost x <= Cost y = x <= y
    Cost _ <= Infinity = True
    Infinity <= Cost _ = False
    Infinity <= Infinity = True

addCost :: Num a => Cost a -> Cost a -> Cost a
addCost (Cost x) (Cost y) = Cost (x + y)
addCost _ _ = Infinity

-- aliases
type Pos = (Int, Int)
type CostMatrix = Array Pos (Cost Int)
type CostMap = M.Map Pos (Cost Int)
type NeighborTbl = Array Pos [Pos]
type PQueue = H.LeftistHeap Node

-- Node for priority queue (key: c)
data Node = Node {c :: Cost Int, p :: Pos} deriving (Show)

instance Eq Node where
    (==) :: Node -> Node -> Bool
    (Node x _) == (Node y _) = x == y

instance Ord Node where
    compare :: Node -> Node -> Ordering
    compare (Node x _) (Node y _) = compare x y

fileData :: BS.ByteString
fileData = $(FE.makeRelativeToProject "resources/0083_matrix.txt" >>= FE.embedFile)

parseData :: String -> CostMatrix
parseData str =
    listArray ((1, 1), (nRow, nCol)) $ concat matrix
  where
    matrix =
        map
            (map (Cost . (read :: String -> Int)) . wordsWhen (\c -> c == ','))
            (lines str)
    nRow = length matrix
    nCol = length $ headExn matrix

    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =
        aux (break p s) []
      where
        aux :: (String, String) -> [String] -> [String]
        aux ("", "") acc = reverse acc
        aux ("", tl) acc = aux (break p (drop 1 tl)) acc
        aux (hd, tl) acc = aux (break p (drop 1 tl)) (hd : acc)

findMinCost :: CostMatrix -> Pos -> Pos -> Int
findMinCost matrix start goal
    | Cost v <- dijkstra (initialPQ, initialCostMap) M.! goal = v
    | otherwise = error "fatal error"
  where
    ix :: ((Int, Int), (Int, Int))
    ix = bounds matrix

    initialPQ :: PQueue
    initialPQ =
        H.insert (Node (matrix ! start) start) H.empty

    initialCostMap :: CostMap
    initialCostMap =
        M.insert
            start
            (matrix ! start)
            ( M.fromList
                [ ((r, c), Infinity)
                | r <- [fst $ fst ix .. fst $ snd ix]
                , c <- [snd $ fst ix .. snd $ snd ix]
                ]
            )

    neighborTbl :: NeighborTbl
    neighborTbl =
        listArray ix $
            map
                ( \(r, c) ->
                    filter
                        (inRange ix)
                        [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
                        -- If the following list is used instead, this solver
                        -- will be able to address the problem 81.
                        -- [(r + 1, c), (r, c + 1)]
                )
                [ (r, c)
                | r <- [fst $ fst ix .. fst $ snd ix]
                , c <- [snd $ fst ix .. snd $ snd ix]
                ]

    dijkstra :: (PQueue, CostMap) -> CostMap
    dijkstra (pq, cmap)
        | H.isEmpty pq == True =
            cmap
        | otherwise =
            let Node{c, p} = H.peek pq -- the node which has minimum cost
                nbrs = neighborTbl ! p
             in dijkstra (updateNeighbors (H.delete pq, cmap) c nbrs)

    updateNeighbors ::
        (PQueue, CostMap) ->
        Cost Int -> -- total cost of node 'p' from the starting position
        [Pos] -> -- neighbor nodes of node 'p'
        (PQueue, CostMap)
    updateNeighbors (pq, cmap) _ [] = (pq, cmap)
    updateNeighbors (pq, cmap) c (x : xs)
        | addCost c (matrix ! x) < (cmap M.! x) =
            let new_cost = addCost c (matrix ! x)
             in updateNeighbors (H.insert (Node new_cost x) pq, M.insert x new_cost cmap) c xs
        | otherwise =
            updateNeighbors (pq, cmap) c xs

compute :: String
compute =
    show $ findMinCost matrix start goal
  where
    matrix = parseData (BS.unpack fileData)
    (start, goal) = bounds matrix

solve :: String
solve = compute
