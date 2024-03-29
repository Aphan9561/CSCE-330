-- Note: Your file must be free of typing errors. If your file can not
-- be loaded into ghci, then you will get 0 point. Please read the instructions
-- for each problem carefully. Failure to follow the instructions may result in
-- 0 point.

-- Problem 1 (2 points). Write down definitions that have the following types;
-- it does not matter what the definitions actually do as long as they are type correct.

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f x y  = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y


-- Problem 2 (2 points): redefine the map function using foldr.
map' :: (a -> b) -> [a] -> [b]
map' f l = foldr (\ x r -> f x : r) [] l


-- Problem 3 (2 points): redefine the filter function using foldr.
filter' :: (a -> Bool) -> [a] -> [a]
filter' f l =
  foldr (\ x r -> if f x then x:r else r) [] l


-- Problem 4 (2 points): Define the following function altMap that
-- alternately applies its two argument functions to successive elements in a list,
-- in turn about order. For example:
-- > altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g (x:[]) = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

-- Problem 5 (2 points): 
-- Use pattern matching and recursion to define a function 'halve'
-- that splits a list
-- into two halves whose lengths differ by at most one.
-- For example:
-- > halve [0,1,2,3,4]
-- ([0,2,4],[1,3])
-- > halve [0,1,2,3,4, 5]
-- ([0,2,4],[1,3,5])

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve (x:[]) = ([x], [])
halve (x:y:ys) =
  let (l, r) = halve ys
  in (x:l, y:r)


-- Problem 6 (2 points)
-- Use pattern matching and recursion to define the function merge :: Ord a => [a] -> [a] -> [a] that
-- merges two sorted lists to give a single sorted list. For example:
-- > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
-- > merge [1,5,5] [3,8]
-- [1,3,5,5,8]

merge :: Ord a => [a] -> [a] -> [a]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) =
  if x <= y then x:merge xs (y:ys)
  else y:merge (x:xs) ys

-- Problem 7 (2 points)
-- Using the 'halve' and 'merge' functions, pattern matching and recursion to
-- define a function 'msort' that implements merge sort,
-- in which the empty list and singleton lists are already
-- sorted, and any other list is sorted by merging together the two lists that
-- result from sorting the two halves of the list separately.

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort l =
  let (l1, l2) = halve l
      l1' = msort l1
      l2' = msort l2
  in merge l1' l2'

-- > msort [5,9,8,6,5,4,3,2,1]
-- [1,2,3,4,5,5,6,8,9]  




