-- Problem 1 (2 points): Use pattern matching and recursion the following 'myDrop' function
-- that drops the first n elements from a list.
-- We assume the input number is always greater or
-- equaled to 0. 

myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n l | n == 0 = l
myDrop n (x:xs) | n > 0 = myDrop (n-1) xs

-- For your convenient, your implementation
-- of myDrop should have the following
-- behavior from the interpreter.

-- > myDrop 3 [1,2,3,4,5]
-- [4,5]
-- > myDrop 3 []
-- []
-- > myDrop 3 [1,2]
-- []
-- > myDrop 0 [1,2]
-- [1,2]
-- > myDrop 4 [1,2]
-- []

-- Problem 2 (2 points): Use pattern matching and recursion the following 'myConcat' function
-- that concatenates a list of list.
-- You may use the built-in append function (++)
-- from Haskell. 
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

-- e.g.
-- > myConcat [[1,2], [3,4], [5,6]]
-- [1,2,3,4,5,6]

-- Problem 3 (2 points): Produce a list with n identical elements.
-- We may assume the input number is always greater
-- or equaled to 0. 
duplicate :: Int -> a -> [a]
duplicate n x | n == 0 = []
duplicate n x | n > 0 = x:duplicate (n-1) x

-- For example, 
-- > duplicate 3 'a'
-- "aaa"
-- > duplicate 3 '1'
-- "111"
-- > duplicate 3 1
-- [1,1,1]
-- > duplicate 0 1
-- []

-- Problem 4 (2 points): Define the following 'myElem' function that decides if a value is an element of a list.

myElem :: Eq a => a -> [a] -> Bool
myElem x [] = False
myElem x (y:ys) =
  if x == y then True else
    myElem x ys

-- For example, 
-- > myElem 1 [1,2,3,4]
-- True
-- > myElem 5 [1,2,3,4]
-- False    

-- Problem 5 (3 points): Define the following 'myMaximum' function that return the maximum element in a list.

myMaximum :: (Ord a) => [a] -> Maybe a  
myMaximum [] = Nothing
myMaximum (x:xs) =
  case myMaximum xs of
    Nothing -> Just x
    Just y -> 
      if x > y then Just x
      else Just y

-- For example.
-- > myMaximum []
-- Nothing
-- > myMaximum [1,2,3,4]
-- Just 4

-- Problem 6 (1 point, but proceed with cautious).
-- Recall that
-- fibonacci sequence is defined as the following:
-- 0, 1, 1, 2, 3, 5 , ...
-- where the n-th number in the sequence
-- is the sum of previous two numbers.
-- Define the following 'myFibs' that
-- return the infinite list of fibonacci sequence. 
-- Note: You solution must return (myFibs !! 100)
-- within 1 second. Your definition must consits
-- of functions and concepts that we learned
-- from the class. You will not get point if
-- you uses concepts that are outside of the class.

myFibs :: [Integer]
myFibs = map (nthFib 0 1) [0 ..]

nthFib :: Integer -> Integer -> Integer -> Integer
nthFib x y n | n == 0 = x
nthFib x y n | n == 1 = y
nthFib x y n | n > 1 = nthFib y (x+y) (n-1)


-- For example (Note that !! is an index function that obtains the nth element in a list):
-- > myFibs !! 3
-- 2
-- > myFibs !! 4
-- 3
-- > myFibs !! 5
-- 5
-- > myFibs !! 6
-- 8
-- > myFibs !! 100
-- > 354224848179261915075




