module ProgLang_hw1 where

-- 1: Create a function that counts the occurences of a character in a string.
count c [] = 0
count c s = (if (last s == c) then 1 else 0) + count c (take (length s - 1) s)

-- 2: Create a function that reverses a list.
--  Put the last element of the list at the front and then call the function on the rest of the list.
rev [] = []
rev l = last l : rev (take (length l - 1) l)

-- 3: create a function that sorts a list.
--  This is selection sort.
--  Put the minimum value at the beginning, sort the rest of the list.
sort [] = []
sort (x:xs) = sort [a | a <- xs, a <= x] ++ [x] ++ sort [a | a <- xs, a > x]

--4: polynomial madness!
type Polynomial = [Double]

--4a: Evaluate a polynomial.
-- Take the last value in the list, and multiply it by x^len(list)-1.
-- So with a list of [1,2,3], recursive step would return (3x^2) + ev([1,2])
-- An empty list evaluates to 0 to create the base case.
ev :: Polynomial -> Double -> Double
ev [] x = 0
ev p x = (x ^ (length p - 1)) * last p + ev (take (length p - 1) p) x

--4b: Add 2 polynomials.
-- Add the first element of the lists, then call the function on the rest of the lists.
-- With lists [1,2,3] and [4,5,6], this would return 5 : addP ([2,3], [5,6])
-- 2 empty lists return an empty list for the base case.
addP :: Polynomial -> Polynomial -> Polynomial
addP [p] [] = [p]
addP [] [p] = [p]
addP u w = (head u + head w) : addP (drop 1 u) (drop 1 w)

--4c: Scale a polynomial.
-- Multiply the first element by the scale, then call the function on the rest of the lists.
-- With a list [1,2,3] and scale 2, return 2 * 1 : [2,3]
-- Empty list, no matter the scale, returns an empty list for the base case.
scale :: Double -> Polynomial -> Polynomial
scale x [] = []
scale x p = head p * x : scale x (drop 1 p)

--4e: Derive a polynomial.
-- Take the last value and multiply it by the len(list)-1. This is because of the power rule.
-- Append that value to the end of the function run on the rest of the list.
-- With a list [1,2,3], this returns der([1,2]) : 6
-- A list of length 1 evaluates to empty to create the base case. This is because the derivative of a constant is 0.
der :: Polynomial -> Polynomial
der [x] = []
der p = der (take (length p - 1) p) ++  [(last p * fromIntegral (length p - 1))]

--4 bonus: Integrate a polynomial.
intP [] c = [c]
intP p c = intP (take (length p - 1) p) c ++ [(last p) / fromIntegral (length p)]