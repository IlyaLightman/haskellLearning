curriedFunctionEx = (max 4) 5

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
mn = multTwoWithNine 2 3

multWithEighteen = multTwoWithNine 2
me = multWithEighteen 10

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

at1 = applyTwice (+3) 10
at2 = applyTwice (++ " HAHA") "Hey"
at3 = applyTwice ("HAHA " ++) "Hey"
at4 = applyTwice (multThree 2 2) 9
at5 = applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zw1 = zipWith' (+) [4,2,5,6] [2,6,2,3]
zw2 = zipWith' max [6,3,2,1] [7,3,1,5]
zw3 = zipWith' (++) ["foo ", "bar ", "baz "]
                    ["fighters", "hoppers", "aldrin"]
zw4 = zipWith' (*) (replicate 5 2) [1..]
zw5 = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]]
                              [[3,2,2],[3,4,5],[5,4,3]]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

fp1 = flip'' zip [1, 2, 3, 4, 5] "Hello"
fp2 = zipWith' (flip'' div) [2, 2..] [10, 8, 6, 4, 2]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

m1p = map (+3) [1, 5, 3, 1, 6]
m2p = map (++ "!") ["BIFF", "BANG", "POW"]
m3p = map (replicate 3) [3..6]
m4p = map (map (^2)) [[1, 2], [3, 4, 5, 6], [7, 8]]
m5p = map fst [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)]

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

f1r = filter (> 3) [1, 5, 3, 2, 1, 6, 4, 3, 2, 1]
f2r = filter (== 3) [1, 2, 3, 4, 5]
f3r = filter even [1..10]
f4r = let notNull x = not (null x) in filter
      notNull [[1, 2, 3], [], [3, 4, 5], [2, 2], [], [], []]
f5r = filter (`elem` ['A'..'Z']) "IdontLIKETOADS"

quicksortFilter :: (Ord a) => [a] -> [a]
quicksortFilter [] = []
quicksortFilter (x:xs) =
    let less = quicksortFilter (filter (<= x) xs)
        grtr = quicksortFilter (filter (> x) xs)
    in less ++ [x] ++ grtr

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

odds1 = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))
odds2 = sum (takeWhile (< 10000) [n^2 | n <- [1..], odd (n^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

listOfFuns = map (*) [0..]
lof = (listOfFuns !! 4) 5

numLongChains' :: Int
numLongChains' =
    length (filter (\xs -> length xs > 15) (map chain [1..100]))

zw = zipWith
    (\a b -> (a * 30 + 3) / b) [5, 4, 3, 2, 1] [1, 2, 3, 4, 5]

mp = map
    (\(a, b) -> a + b) [(1, 2), (3, 5), (6, 3), (2, 6), (2, 5)]

addThree :: (Num a) => a -> a -> a-> a
addThree x y z = x + y + z

addThreeEquivalent :: (Num a) => a -> a -> a -> a
addThreeEquivalent = \x -> \y -> \z -> x + y + z

flip''' ::  (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys =
    foldl (\acc x -> if x == y then True else acc) False ys

mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs

-- ++ much more expensive than :
mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

foldMaximum :: (Ord a) => [a] -> a
foldMaximum = foldr1 (\x acc -> if x > acc then x else acc)

foldReverse :: [a] -> [a]
foldReverse = foldl (\acc x -> x : acc) []

foldReverse' :: [a] -> [a]
foldReverse' = foldl (flip (:)) []

foldProduct :: (Num a) => [a] -> a
foldProduct = foldr1 (*)

foldFilter :: (a -> Bool) -> [a] -> [a]
foldFilter p = foldr (\x acc -> if p x then x : acc else acc) []

foldHead :: [a] -> a
foldHead = foldr1 (\x _ -> x)

foldLast :: [a] -> a
foldLast = foldl1 (\_ x -> x)

sn1 = scanl (+) 0 [3, 5, 2, 1]
sn2 = scanr (+) 0 [3, 5, 2, 1]
sn3 = scanl1 (\acc x -> if x > acc then x else acc)
             [3, 4, 5, 3, 7, 9, 2, 1]
sn4 = scanl (flip (:)) [] [3, 2, 1]

sqrtSums :: Int
sqrtSums = length
    (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1

dollar :: (a -> b) -> a -> b
f `dollar` x = f x

dra = sum (filter (> 10) (map (* 2) [2..10]))
drb = sum $ filter (> 10) $ map (* 2) [2..10]

trt = map ($ 3) [(4 +), (10 *), (^2), sqrt]

dot :: (b -> c) -> (a -> b) -> a -> c
f `dot` g = \x -> f (g x)

negLa = map (\x -> negate (abs x)) [5, -3, -6, 7, -3, 2, -19, 24]
negDt = map (negate . abs) [5, -3, -6, 7, -3, 2, -19, 24]

nstLa = map (\xs -> negate (sum (tail xs)))
    [[1..5], [3..6], [1..7]]
nstDt = map (negate . sum . tail) [[1..5], [3..6], [1..7]]

srm = sum (replicate 5 (max 6.7 8.9))
srmDt = (sum . replicate 5 . max 6.7) 8.9
srmDd = sum . replicate 5 . max 6.7 $ 8.9

rpzCl = replicate 100 (product (map (* 3)
    (zipWith max [1, 2, 3, 4, 5] [4, 5, 6, 7, 8])))
rpzDd = replicate 100 . product . map (* 3)
    . zipWith max [1, 2, 3, 4, 5] $ [4, 5, 6, 7, 8]

fn x = ceiling (negate (tan (cos (max 50 x))))

fnDot = ceiling . negate . tan . cos . max 50

oddSquareSum' :: Integer
oddSquareSum' =
    sum . takeWhile (< 10000) . filter odd . map (^2) $ [1..]

oddSquareSumBetter :: Integer
oddSquareSumBetter =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (< 10000) oddSquares
    in sum belowLimit
