module Bowling (toFrames) where

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

score :: [Frame] -> Int
score [] = 0
score (Strike:[]) = 10
score (Strike:next:[]) = 10 + 2 * score1 next
score (Strike:Strike:a:rest) = 20 + score1 (head tltl) + score rest
score (Strike:rest) = 10 + score1 (head rest) + score rest
score (Spare _ _:[]) = 10
score (Spare _ _:rest) = 10 + score1 (head rest) + score rest
score (Rolls a b:rest) = a + b + score rest

score1 :: Frame -> Int
score1 Strike = 10
score1 (Spare _ _) = 10
score1 (Rolls a b) = a + b
score1 (PartialFrame a) = a

scoreAccum :: Frame -> (Int, Maybe Int -> Maybe Int -> Maybe Int)
scoreAccum Strike		  = (10,  liftM2 (+))
scoreAccum (Spare a b)	  = (10,  const)
scoreAccum (Rolls a b)	  = (a+b, \_ _ -> Nothing)
scoreAccum (PartialFrame a) = (a ,\_ _ -> Nothing)

applySnd :: (a,b -> c) -> b -> (a,c)
applySnd (a,f) b = (a, f b)

score = map scoreAccum
