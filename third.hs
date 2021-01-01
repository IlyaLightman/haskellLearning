lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck. pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectorsBetter :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectorsBetter (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

xs = [(1, 3), (4, 3), (2, 4), (5, 3), (5, 6), (3, 1)]
xss = [ a + b | (a, b) <- xs ]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x : _) = x

h = head' "Hello!"

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: "
                ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  | bmi <= 18.5 = "You are underweight!"
  | bmi <= 25.0 = "You are supposedly normal"
  | bmi <= 30.0 = "You are fat!"
  | otherwise = "You are a whale!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
  | weight / height ^ 2 <= 18.5 = "You are still underweight"
  | weight / height ^ 2 <= 25.0 = "You are supposedly normal"
  | weight / height ^ 2 <= 30.0 = "You are still fat, bro :("
  | otherwise = "You are whale!"

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

bmiTellBetter :: (RealFloat a) => a -> a -> String
bmiTellBetter weight height
  | bmi <= skinny = "You are underweight"
  | bmi <= normal = "You are supposedly normal"
  | bmi <= fat    = "You are fat :("
  | otherwise     = "You are whale!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

al = 4 * (let a = 9 in a + 1) + 2
sq = [let square x = x * x in (square 5, square 3, square 2)]
ab = (let a = 100; b = 200; c = 300 in a * b * c,
      let foo = "Hey "; bar = "there!" in foo ++ bar)
ac = (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 1.0]

headCaseExpression :: [a] -> a
headCaseExpression xs = case xs of [] -> error "No head for empty lists!"
                                   (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describleList' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."
