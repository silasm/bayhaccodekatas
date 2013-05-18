ignore :: Int -> Int -> Int
ignore _ _ = 0

scoreAccum' :: [Int] -> [(Int, Int -> Int -> Int)]
scoreAccum' []    = []
--strike
scoreAccum' (10:xs) = (10, (+)) : scoreAccum' xs
--partial
scoreAccum' (a:[])  = [(a,ignore)]
scoreAccum' (a:b:xs)
	--spare
	|a + b == 10  = (10,const) : scoreAccum' xs
	--non-spare
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

display :: [Int] -> String
display [] = ""
display (10:xs) = "X " ++ display xs
display (a:[])	= show a
display (a:b:xs)
	|a + b == 10 = display' a ++ "/" ++ display xs
	|otherwise   = display' a ++ display' b
	where
		display' 0 = "-"
		display' a = show a
