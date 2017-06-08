--problem1
myLast :: [a] -> a
myLast []     = error "Empty set"
myLast [x]    = x
myLast (x:xs) = myLast xs

--problem2
myButLast :: [a] -> a
myButLast []  = error "Empty set"
myButLast [x] = error "Singleton set"
myButLast (x:xs)
  | length xs == 1  = x
  | otherwise       = myButLast xs

--problem3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:xs) i
  | i < 1      = error "Index out of bounds"
  | i == 1     = x
  | otherwise  = elementAt xs (i-1)

--problem4
myLength :: [a] -> Int
myLength []     = 0
myLength (x:xs) = 1 + myLength xs

--problem5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

--problem6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []   = True
isPalindrome [_]  = True
isPalindrome xs   = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

--problem7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
myFlatten :: NestedList a -> [a]
myFlatten (List [])     = []
myFlatten (Elem x)      = [x]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

--problem8
compress :: (Eq a) => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:xs)
  | x == head xs  = compress xs
  | otherwise     = x : compress xs

--problem9
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:first) : pack rest
  where
    getReps [] = ([], [])
    getReps (y:ys)
      | y == x     = let (f, r) = getReps ys in (y:f, r)
      | otherwise  = ([], (y:ys))
    (first, rest) = getReps xs

--problem10
encode :: (Eq a) => [a] -> [(Int,a)]
encode xs = map (\ys@(y:_) -> (length ys, y)) $ pack xs

--problem11
data EncodeModified a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [EncodeModified a]
encodeModified xs = map (\(k,x) -> if k > 1 then Multiple k x else Single x) $ encode xs

--problem12
decodeModified :: [EncodeModified a] -> [a]
decodeModified xs =  foldl (\acc x -> acc ++ x) [] $ map decode xs
  where decode (Multiple i x) = repeat' x i
          where
            repeat' x k
              | k > 0      = x : repeat' x (k-1)
              | otherwise  = []
        decode (Single x) = [x]

--problem13
encodeDirect :: (Eq a) => [a] -> [EncodeModified a]
encodeDirect [] = []
encodeDirect (x:xs@(y:ys))
  | x==y       = Multiple (1 + length(first)) x : encodeDirect rest
  | otherwise  = Single x : encodeDirect xs
  where
    getReps [] = ([], [])
    getReps (y:ys)
      | y == x     = let (f,r) = getReps ys in (y:f, r)
      | otherwise  = ([], (y:ys))
    (first,rest) = getReps xs

--problem14
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x : x : dupli xs

--problem15
repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) k = replicate k x ++ repli xs k

--problem16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs k = dropEvery' 1 xs
  where
    dropEvery' _ [] = []
    dropEvery' i pp@(p:xp)
      | mod i k == 0  = dropEvery' (i+1) xp
      | otherwise     = [p] ++ (dropEvery' (i+1) xp)

--problem17
split :: [a] -> Int -> ([a],[a])
split [] _ = ([],[])
split pp@(x:xs) k
  | k > 0 = (x:ys, zs)
  | otherwise = ([],pp)
  where
    (ys,zs) = split xs (k-1)

--problem18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice pp@(x:xs) i f
  | (i > f) || ( i > length pp) || (f < 1) = []
  | i == 1 = x : (slice xs 1 (f-1))
  | otherwise = slice xs (i-1) (f-1)

--problem19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate l@(x:xs) k
  | k == 0     = l
  | k == 1     = xs ++ [x]
  | k < 1      = rotate l (length l + k)
  | otherwise  = rotate (xs ++ [x]) (k-1)

--problem20
removeAt :: Int -> [a] -> (Maybe a,[a])
removeAt _ []      = (Nothing, [])
removeAt 1 (x:xs)  = (Just x,xs)
removeAt k (x:xs)  = (a,x:r)
  where (a,r) = removeAt (k-1) xs

--problem21
insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x xs@(y:ys) k
  | k <= 1 = x:xs
  | otherwise = y:insertAt x ys (k-1)

--problem22
range :: Int -> Int -> [Int]
range i f
  | i > f = error "Wrong range"
  | i == f = [i]
  | otherwise = i:range (i+1) f

--problem23
