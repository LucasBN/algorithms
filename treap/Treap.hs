module Treap where

data Treap a = Empty | Node (Treap a) a Int (Treap a)

member :: Ord a => a -> Treap a -> Bool
member _ Empty = False
member y (Node l x _ r)
  | y == x = True
  | y < x  = member y l
  | x > y  = member y r

insert :: Ord a => a -> Int -> Treap a -> Treap a
insert x p Empty = Node Empty x p Empty
insert x p n@(Node l y q r)
  | x == y = n
  | x < y  = mkLNode (insert x p l) y q r
  | x > y  = mkRNode l y q (insert x p r)

mkLNode :: Treap a -> a -> Int -> Treap a -> Treap a
mkLNode Empty x p r = Node Empty x p r
mkLNode l@(Node ll x p lr) y q r
  | q <= p    = Node l y q r
  | otherwise = Node ll x p (Node lr y q r)

mkRNode :: Treap a -> a -> Int -> Treap a -> Treap a
mkRNode l x p Empty = Node l x p Empty
mkRNode l x p r@(Node rl y q rr)
  | p <= q    = Node l x p r
  | otherwise = Node (Node l x p rl) y q rr 