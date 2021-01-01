doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

numbers1 = [1, 2, 3, 4, 5]
numbers2 = 12 : [10, 9, 8, 7, 6]
numbers3 = numbers1 ++ numbers2

cat = "Little " ++ ['c', 'a', 't']
acat = 'A' : ' ' : cat
c = acat !! 9
l = length acat
az = ['a', 'd' .. 'z'] ++ ['0' .. '9'] ++ ['e', 'd' .. 'a']
tfo = take 24 [13, 26 ..]
lol = take 100 (cycle "LOL ")
desat = replicate 10 10

v1 = [x * 2 | x <- [1 .. 10], x * 2 >= 12]
v2 = [x | x <- [50 .. 100], x `mod` 7 == 3]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
bb = boomBangs [0..30]
mpr = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
sp = [ x * y | x <- [2, 5, 10], y <- [8, 10, 11], x * y > 30]

nouns = ["hobo", "frog", "pope"]
adjectives = ["lazy", "grouchy", "scheming"]
adjn = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]
frogs = removeNonUppercase "IdontLIKEFROGS"
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
xxss = [ [x | x <- xs, even x ] | xs <- xxs]

tuples = [(0, 5), (10, 8), (7, 1)]
chessDiagonal = zip ['A' .. 'H'] [1 .. 8]

rightTriangles = [ (a, b, c) | a <- [1 .. 10], b <- [1 .. 10], c <- [1 .. 10], a^2 + b^2 == c^2, a + b + c == 24]
