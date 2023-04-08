{-# LANGUAGE BlockArguments #-}
module Tree where

data Tree a = Tip | Node Int (Tree a) a (Tree a) deriving (Show, Eq)

height :: Tree a -> Int
height Tip            = 0
height (Node h _ _ _) = h

mkNode :: Tree a -> a -> Tree a -> Tree a
mkNode l v r = Node (h + 1) l v r
  where h = max (height l) (height r)

insert :: Ord a => a -> Tree a -> Tree a
insert x Tip = mkNode Tip x Tip
insert x t@(Node _ l y r)
  | x == y    = t
  | x < y     = balancel (insert x l) y r
  | otherwise = balancer l y (insert x r)

balancel :: Tree a -> a -> Tree a -> Tree a
balancel l x r
  | height l - height r <= 1 = mkNode l x r
  | otherwise = case l of
      (Node _ ll _ rl)
        | height ll >= height rl -> rotr (mkNode l x r)
        | otherwise              -> rotr (mkNode (rotl l) x r)
      _ -> error ""

balancer :: Tree a -> a -> Tree a -> Tree a
balancer l x r
  | height r - height l <= 1 = mkNode l x r
  | otherwise = case r of
      (Node _ lr _ rr)
        | height lr >= height rr -> rotl (mkNode l x r)
        | otherwise              -> rotl (mkNode l x (rotr r))
      _ -> error ""

rotr :: Tree a -> Tree a
rotr (Node _ (Node _ p x q) y r) = mkNode p x (mkNode q y r)
rotr t = t

rotl :: Tree a -> Tree a
rotl (Node _ l y (Node _ p x q)) = mkNode (mkNode l y p) x q
rotl t = t