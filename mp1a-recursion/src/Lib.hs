--- Getting Started
--- ===============

--- Relevant Files
--- --------------
{- HLINT ignore "Use foldr" -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake
mytake :: Int -> [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
mytake _ [] = []
mytake firstN _ | firstN <= 0 = []
mytake firstN (x:xs) = x : mytake (firstN - 1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n list | n <= 0 = list
mydrop n (x:xs) = mydrop (n-1) xs

--- ### rev
rev :: [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
rev [x] = [x]
rev list = aux list []
    where
        aux [] l = l
        aux (x:xs) l = aux xs (x:l)

--- ### app
app :: [a] -> [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
app [] l = l
app l [] = l
app (x:xs) l = x : app xs l


--- ### inclist
inclist :: Num a => [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
inclist [] = []
inclist (x:xs) = x+1 : inclist xs

--- ### sumlist
sumlist :: Num a => [a] -> a
-- don't forget to put the type declaration or you will lose points!
sumlist xs = aux xs 0
    where
        aux [] acc      = acc
        aux (x:xs) acc  = aux xs acc+x

--- ### myzip
myzip :: [a] -> [b] -> [(a,b)]
-- don't forget to put the type declaration or you will lose points!
myzip l [] = []
myzip [] l = []
myzip (x:xs) (y:ys) = (x, y) : myzip xs ys

--- ### addpairs
addpairs :: (Num a) => [a] -> [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
addpairs l [] = []
addpairs [] l = []
addpairs (x:xs) (y:ys) = x + y : addpairs xs ys

--- ### ones
ones :: [Integer]
-- don't forget to put the type declaration or you will lose points!
ones = 1 : ones

--- ### nats
nats :: [Integer]
-- don't forget to put the type declaration or you will lose points!
nats = aux 0
    where
        aux prev = prev : aux (prev + 1)

--- ### fib
fib :: [Integer]
-- don't forget to put the type declaration or you will lose points!
fib = 0 : 1 : aux 0 1
    where
        aux prev cur = (prev + cur) : aux cur (prev + cur)

--- Set Theory
--- ----------

--- ### add```````````````````````````````````````````````````````````````````````````````````````
add :: Ord a => a -> [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
add val [] = [val]
add val n@(x:xs)
    | val < x  = val : n
    | val == x  = n
    | otherwise = x : add val xs

-- add val [x]
--     | val > x   = [x, val]
--     | otherwise = [val, x]
-- add val n@(x1:x2:xs)
--     | val > x2 = x1 : add val (x2:xs)
--     | val > x1 = x1 : val : x2 : xs
--     | otherwise = n

--- ### union
union :: Ord a => [a] -> [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
union a [] = a
union [] b = b
union a@(x:xs) b@(y:ys)
    | x < y     = x : union xs b
    | y < x     = y : union a ys
    | otherwise = x : union xs ys

--- ### intersect
intersect :: Ord a => [a] -> [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
intersect a [] = []
intersect [] b = []
intersect a@(x:xs) b@(y:ys)
    | x < y     = intersect xs b
    | y < x     = intersect a ys
    | otherwise = x : intersect xs ys

--- ### powerset
powerset :: Ord a => [a] -> [[a]]
-- don't forget to put the type declaration or you will lose points!
powerset xs = trav xs []
    where
        trav [] cur = [cur]
        trav level@(x:xs) cur = union (trav xs cur) (trav xs (add x cur))

--- Higher Order Functions
--- ----------------------

mymap _ [] = []
mymap op (x:xs) = op x : mymap op xs

myfoldr op init [] = init
myfoldr op init (x:xs) = op init $ myfoldr op x xs

--- ### inclist'
inclist' :: Num a => [a] -> [a]
-- don't forget to put the type declaration or you will lose points!
myinc a = a + 1
inclist' = mymap myinc

--- ### sumlist'
sumlist' :: Num a => [a] -> a
-- don't forget to put the type declaration or you will lose points!
myadd a b = a + b
sumlist' = myfoldr myadd 0




--- ### sumlist
-- don't forget to put the type declaration or you will lose points!
