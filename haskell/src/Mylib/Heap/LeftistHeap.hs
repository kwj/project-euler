module Mylib.Heap.LeftistHeap (
    LeftistHeap,
    module Mylib.Heap.Internal
) where

import Mylib.Heap.Internal ( Heap(..) )

data LeftistHeap a = Empty | Tree Int a (LeftistHeap a) (LeftistHeap a)

rank :: Ord a => LeftistHeap a -> Int
rank Empty = 0
rank (Tree n _ _ _) = n

makeTree :: Ord a => a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeTree x a b =
    if rank a >= rank b
        then Tree (rank b + 1) x a b
        else Tree (rank a + 1) x b a

instance Heap LeftistHeap where
    empty :: Ord a => LeftistHeap a
    empty = Empty

    isEmpty :: Ord a => LeftistHeap a -> Bool
    isEmpty Empty = True
    isEmpty _ = False

    merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
    merge h Empty = h
    merge Empty h = h
    merge h1@(Tree _ x l1 r1) h2@(Tree _ y l2 r2) =
        if x <= y
            then makeTree x l1 (merge r1 h2)
            else makeTree y l2 (merge r2 h1)

    insert :: Ord a => a -> LeftistHeap a -> LeftistHeap a
    insert x h = merge (Tree 1 x Empty Empty) h

    peek :: Ord a => LeftistHeap a -> a
    peek Empty = error "empty heap"
    peek (Tree _ x _ _) = x

    delete :: Ord a => LeftistHeap a -> LeftistHeap a
    delete Empty = error "empty heap"
    delete (Tree _ _ a b) = merge a b
