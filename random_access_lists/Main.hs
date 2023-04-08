{-# LANGUAGE BangPatterns #-}
module Main where

-- There is a resemblance between Peano numbers and Standard Lists: 
data Peano = Zero | Succ Peano
data List a = Empty | Cons a (List a)

data Tree a = Tip | Leaf a | Fork Int (Tree a) (Tree a) deriving Show
newtype RAList a = RAList [Tree a] deriving Show

mkList :: [a] -> List a
mkList = foldr Cons Empty

listLookup :: List a -> Int -> a
listLookup xs k = lookup' xs k 0
  where
    lookup' Empty _ _ = error "nope"
    lookup' (Cons y ys) k i
      | k == i    = y
      | otherwise = lookup' ys k (i + 1)

-- O(1)
treeLength :: Tree a -> Int
treeLength (Fork h _ _) = h
treeLength (Leaf _)     = 1
treeLength _            = 0

mkFork :: Tree a -> Tree a -> Tree a
mkFork l r = Fork (treeLength l + treeLength r) l r

flattenTree :: Tree a -> [a]
flattenTree Tip = []
flattenTree (Leaf x) = [x]
flattenTree (Fork _ l r) = flattenTree l ++ flattenTree r

treeLookup :: Tree a -> Int -> a
treeLookup Tip _        = error "No values in tip"
treeLookup (Leaf x) 0   = x
treeLookup (Leaf _) k   = error "Only 1 element in a leaf"
treeLookup (Fork _ l r) k
  | k < m     = treeLookup l k
  | otherwise = treeLookup r (k - m)
  where m = treeLength l

-- PRE: List has 2^n elements
mkTree :: [a] -> Tree a
mkTree []  = Tip
mkTree [x] = Leaf x
mkTree xs  = mkFork (mkTree l) (mkTree r)
  where (l, r) = splitAt (length xs `div` 2) xs

mkRAList :: [a] -> RAList a
mkRAList xs = RAList [go i j xs | (i, j) <- zip dc (scanl (+) 0 dc)]
  where
    go x y list = mkTree ((take x . drop y) list)
    dc  = decompose (length xs)

    decompose :: Int -> [Int]
    decompose 0 = []
    decompose k = decompose (k - 2^m) ++ [2^m]
      where m = floor (logBase 2 (fromIntegral k))

raListLookup :: RAList a -> Int -> a
raListLookup (RAList []) _ = error "empty list"
raListLookup (RAList (t:ts)) k
  | m == 0    = raListLookup (RAList ts) k
  | k < m     = treeLookup t k
  | otherwise = raListLookup (RAList ts) (k - m)
  where m = treeLength t

main :: IO ()
main = return ()