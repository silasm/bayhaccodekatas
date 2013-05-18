module Bowling (toFrames) where

import Control.Monad

{-
 - 9 1		5 3
 - 10+5		8
 -
 - 10		5 3
 - 10+5+3	8
 -
 - 9 1		10		7 2
 - 10+10	10+7+2	9
 - 20		19		9
 -}

data Frame = Strike | Spare Int Int | Rolls Int Int | PartialFrame Int deriving Show

toFrames :: [Int] -> [Frame]
toFrames [] = []
toFrames (x:[])
	|x == 10		= [Strike]
	|otherwise		= [PartialFrame x]
toFrames (x:x':xs)
	|x == 10		= Strike	 : toFrames (x':xs)
	|x + x' == 10	= Spare x x' : toFrames xs
	|otherwise		= Rolls x x' : toFrames xs


ignore :: Int -> Int -> Int
ignore _ _ = 0

scoreAccum :: Frame -> (Int, Int -> Int -> Int)
scoreAccum Strike		    = (10,  (+))
scoreAccum (Spare a b)	    = (10,  const)
scoreAccum (Rolls a b)	    = (a+b, \_ _ -> 0)
scoreAccum (PartialFrame a) = (a ,\_ _ -> 0)

scoreAccum' :: [Int] -> [(Int, Int -> Int -> Int)]
scoreAccum' []    = []
scoreAccum' (10:xs) = (10, (+)) : scoreAccum' xs
scoreAccum' (a:[])  = [(a,ignore)]
scoreAccum' (a:b:xs)
	|a + b == 10  = (10,const) : scoreAccum' xs
	|otherwise	  = (a+b,ignore) : scoreAccum' xs

applySnd :: (a,b -> c) -> b -> (a,c)
applySnd (a,f) b = (a, f b)

score :: [Int] -> [(Int,Int)]
score xs = zipWith applySnd
	(zipWith applySnd (scoreAccum' xs) (tail' xs))
	(tail' $ tail' xs)
		where
			-- tail that starts at the second frame and adds
			-- zeros on the end to preserve list length.
			tail' :: [Int] -> [Int]
			tail' []	   = []
			tail' (10:xs)  = xs ++ [0]
			tail' (a:[])   = [0]
			tail' (a:b:xs) = xs ++ [0,0]
