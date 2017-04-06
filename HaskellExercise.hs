module HaskellExercise where
--1
reverser :: [a] -> [a] -> [a]
reverser a b = (reverse' a) ++ (reverse' b)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (a:as) = (reverse' as) ++ [a]

--2
pairs :: [a] -> [b] -> [c] -> [(a , (b, c))]
pairs [] [] [] = []
pairs (a:as) (b:bs) (c:cs) = (a ,(b , c)):(pairs as bs cs)

--3
firstPairs :: [(a,b)] -> [a]
firstPairs [] = []
firstPairs ((a,b):as) = a:(firstPairs as)

--4
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave (a:as) (b:bs) = [a] ++ [b] ++ (interleave as bs)

--5
twoFunc :: (a -> b) -> (b -> c) -> [a] -> [c]
twoFunc g f [] = []
twoFunc g f (a:as) = f (g a):(twoFunc g f as)

--6
compairs :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
compairs f [] [] = []
compairs f (a:as) (b:bs) = if (f a b) == True then (a, b):(compairs f as bs) else (compairs f as bs)

oneOff :: Int -> Int -> Bool
oneOff a b = if (a - b) == 1 || (b - a) == 1 then True else False

--7
filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (a:as) = (if (f a) then (a:(filter' f as)) else (filter' f as))
