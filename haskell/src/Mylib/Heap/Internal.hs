module Mylib.Heap.Internal (
    Heap (..),
) where

class Heap h where
    empty :: Ord a => h a
    isEmpty :: Ord a => h a -> Bool
    merge :: Ord a => h a -> h a -> h a
    insert :: Ord a => a -> h a -> h a
    peek :: Ord a => h a -> a
    delete :: Ord a => h a -> h a
