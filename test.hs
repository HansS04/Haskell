import Data.List (groupBy, nubBy)

swap :: String -> String
swap str = init (str) ++ "!"

evens :: String -> String
evens [] = []
evens (x : y : xs) = x : evens xs

makeGirl :: String -> String
makeGirl [] = []
makeGirl str
  | last str == 'a' = init (str) ++ "ova"
  | otherwise = str ++ "ova"

swap2 :: String -> Char -> Char -> String
swap2 [] _ _ = []
swap2 (x : xs) c1 c2
  | x == c1 = c2 : swap2 xs c1 c2
  | otherwise = x : swap2 xs c1 c2

odds :: String -> String
odds [] = []
odds (x : y : xs) = y : odds xs

check :: String -> Bool
check str
  | length (filter (== ')') str) == length (filter (== '(') str) = True
  | otherwise = False

interval :: [Int] -> (Int, Int)
interval [] = (0, 0)
interval x = (minimum x, maximum x)

intersection3 :: (Eq a) => [a] -> [a] -> [a] -> [a]
intersection3 xs ys zs = filter (\x -> x `elem` ys && x `elem` zs) xs

powers :: Int -> Int -> [Int]
powers x n = helper x n 0

helper :: Int -> Int -> Int -> [Int]
helper x n a
  | a < n = x ^ a : helper x n (a + 1)
  | otherwise = []

average :: [Double] -> Double
average [] = 0
average x = sum x / fromIntegral (length x)

names :: [(String, String)] -> String -> [String]
names [] _ = []
names ((x, y) : ys) str
  | isSubstr y str = x : names ys str
  | otherwise = names ys str

isSubstr :: String -> String -> Bool
isSubstr _ [] = True
isSubstr str (a : b : bs)
  | a `elem` str && b `elem` str = isSubstr str bs
  | otherwise = False

-- Jednotlivé testy

-- TEST
countries :: [(String, Int)] -> Int -> [String]
countries [] _ = []
countries ((str, n) : xs) y
  | n > y = str : countries xs y
  | otherwise = countries xs y

intersection2 :: (Eq a) => [a] -> [a] -> [a] -> [a]
intersection2 x y z = filter (\x -> x `elem` y && x `elem` z) x

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | x > y = y : merge (x : xs) ys
  | otherwise = x : merge xs ys

-- TEST

select :: [(String, Float)] -> String
select [] = "Seznam je prázdný"
select ((str, f) : xs) = maxValue xs f str

maxValue :: [(String, Float)] -> Float -> String -> String
maxValue [] _ maxStr = maxStr
maxValue ((str, f) : xs) maxF maxStr
  | f > maxF = maxValue xs f str
  | otherwise = maxValue xs maxF maxStr

factorials :: Int -> Int -> [(Int, Int)]
factorials x y
  | x <= y = (x, factorial x) : factorials (x + 1) y
  | otherwise = []

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * factorial (n - 1)

splitWith :: (Eq a) => [a] -> [a] -> [[a]]
splitWith [] _ = []
splitWith (x : xs) y
  | isSeparator x y = splitWith xs y
  | otherwise = ([x] : splitWith xs y)

isSeparator :: (Eq a) => a -> [a] -> Bool
isSeparator _ [] = False
isSeparator x (y : ys)
  | x == y = True
  | otherwise = isSeparator x ys

-- TEST
convert :: [(String, Int, Float)] -> [(String, Float)]
convert [] = []
convert ((str, n, f) : xs) = (str, fromIntegral n * f) : convert xs

replaceByRepeat :: String -> Char -> Int -> String
replaceByRepeat [] _ _ = []
replaceByRepeat (x : xs) c n
  | x == c = replicate n c ++ replaceByRepeat xs c n
  | otherwise = [x] ++ replaceByRepeat xs c n

operace :: [a] -> [(Int, Int)] -> [a]
operace xs [] = xs
operace xs ((index, delka) : rest) =
  let (predek, zbytek) = splitAt index xs
      (prostredek, zadek) = splitAt delka zbytek
      novySeznam = predek ++ reverse prostredek ++ zadek
   in operace novySeznam rest

-- TEST
accounts :: [(String, Int)] -> [String]
accounts [] = []
accounts ((str, n) : xs)
  | n > 0 = str : accounts xs
  | otherwise = accounts xs

mostFrequent :: [Int] -> Int
mostFrequent (x : xs) = mostHelper x xs 1

mostHelper :: Int -> [Int] -> Int -> Int
mostHelper y [] _ = y
mostHelper y (x : xs) n
  | length (filter (== y) xs) > n = mostHelper y xs (length (filter (== y) xs))
  | otherwise = mostHelper x xs n

getHashMap :: (Eq a) => [a] -> (a -> Int) -> [(Int, [a])]
getHashMap mujInput f =
  let klic = map f mujInput
      uniqueKeys = nubBy (\x y -> x == y) klic
      groupedByKey = groupBy (\x y -> f x == f y) mujInput
      vysledek = zip uniqueKeys groupedByKey
   in vysledek

-- TEST

filter' :: [(String, Int)] -> Int -> [String]
filter' [] _ = []
filter' ((x, y) : xs) n
  | y > n = x : filter' xs y
  | otherwise = filter' xs y

positions :: String -> Char -> [Int]
positions str c = helperPos str c 0

helperPos :: String -> Char -> Int -> [Int]
helperPos [] _ _ = []
helperPos (x : xs) c n
  | x == c = n : helperPos xs c (n + 1)
  | otherwise = helperPos xs c (n + 1)

unique :: (Eq a) => [a] -> [a] -> [a] -> [a]
unique x y z = findUnique x y z ++ findUnique y x z ++ findUnique z x y

findUnique :: (Eq a) => [a] -> [a] -> [a] -> [a]
findUnique [] _ _ = []
findUnique (x : xs) y z
  | x `notElem` y && x `notElem` z = x : findUnique xs y z
  | otherwise = findUnique xs y z