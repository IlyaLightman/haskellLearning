import Data.List
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

itse = intersperse 0 [1, 2, 3, 4, 5, 6, 7]
itre = intercalate [0, 0, 0] [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
ite = intercalate " " [intersperse '.' "Hello", "world", "!"]
trpe = transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- сумма 3x2 + 5x + 9, 10x3 + 9 и 8x3 + 5x2 + x - 1
trin = map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
cnct = concat [[3, 4, 5], [2, 3, 4], [2, 1, 1]]
ctmp = concatMap (replicate 4) [1..3]
a1 = any (== 4) [2, 3, 5, 4, 6, 1]
a2 = all (> 4) [1, 2, 3]
a3 = all (`elem` ['A'..'Z']) "IlikeFROGSandTOADS"
a4 = any (`elem` ['A'..'Z']) "IlikeFROGSandTOADS"
itrt = take 10 $ iterate (* 2) 1
itha = take 3 $ iterate (++ "haha") "haha"
splt = splitAt 3 "heymanhowareyou"
slet = let (a, b) = splitAt 3 "foobar" in b ++ a
skwl = takeWhile (> 3) [6, 5, 4, 3 ,2 , 1, 2, 3, 4, 5, 4, 3]
stw = sum $ takeWhile (< 10000) $ map (^3) [1..]
drwl = dropWhile (< 3) [1, 2, 2, 2, 3, 4, 5, 4, 3, 2, 1]
spn = let (fw, rest) = span (/= ' ') "This is a sentence"
      in "First word: " ++ fw ++ ", the rest: " ++ rest
br1 = break (== 4) [1, 2, 3, 4, 5, 6, 7]
br2 = span (/= 4) [1, 2, 3, 4, 5, 6, 7]
srt = sort [8, 5, 3, 2, 1, 6, 4, 2]
grp = group [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 5, 6, 7]
howManyTimes = map (\l@(x:xs) -> (x, length l)) . group . sort $
    [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 5, 6, 7]
ints = inits "w00t"
tils = tails "w00t"

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc)
        False (tails haystack)

srch = search [1, 2] [0, 7, 3, 1, 2, 5, 4]

isnx = "cat" `isInfixOf` "I am a cat burglar"
ispx = "hey" `isPrefixOf` "hey there!"
issx = "there!" `isSuffixOf` "hey there!"

simpliestAssociationListOfPhoneNumbers =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

phoneMap = Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
