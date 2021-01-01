removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

az = "Abrakadabra" `compare` "Zebra"
pa = show 5.334
ar = read "[1,2,3,4]" ++ [3]
tl = read "(3, 'a')" :: (Int, Char)

minInt = minBound :: Int
minChar = minBound :: Char
maxBool = maxBound :: Bool
minBool = minBound :: Bool
maxes = maxBound :: (Bool, Int, Char)
dvdct = (20 :: Int, 20 :: Float, 20 :: Double)
x = fromIntegral (length [1, 2, 3, 4]) + 3.2
