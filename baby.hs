import Data.List

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
  then x
  else x*2

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

{-|  Where bindings are a syntactic construct that let you bind to variables at the end of a function
 and the whole function can see them, including all the guards -}

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
 | bmi <= skinny  = "You're underweight"
 | bmi <= normal  = "You're normal weight"
 | bmi <= fat     = "You're fat"
 | otherwise      = "You're obese"
 where bmi = weight / height ^ 2
       ( skinny, normal, fat ) = ( 18.5, 25.0, 30.0 )

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

{-| Let bindings let you bind to variables anywhere and are expressions themselves,
but are very local, so they don't span across guards -}

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list is undefined"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort[a | a <- xs, a <= x]
      biggerSorted = quicksort[a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

--Heigher order functions

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort ( filter (<=x) xs )
      biggerSorted = quicksort ( filter (>x) xs )
  in smallerSorted ++ [x] ++ biggerSorted

--collatz sequence

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x = x : chain (x `div` 2)
  | odd x = x : chain (x * 3 + 1)

numLongChains :: (Integral a) => [a] -> Int
numLongChains x = length (filter isLong (map chain x))
  where isLong xs = length xs > 15


-- Lambdas = Anonymus functions

numLongChains' :: (Integral a) => [a] -> Int
numLongChains' x = length (filter (\xs -> length xs > 15) (map chain x))

--using fold functions - foldr and foldl
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- ++ function is much more expensive than :, so we usually use right folds when we're building up new lists from a list.
-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states in the form of a list.

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- We use takeWhile here instead of filter because filter doesn't work on infinite lists

-- '$' function
fn1 :: Double -> Integer
fn1 = ceiling . negate . tan . cos . max 50

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)


main = do line <- fmap reverse getLine
          putStrLn $ "You said " ++ line ++ " backwards!"
          putStrLn $ "Yes, you really said " ++ line ++ " backwards!"
