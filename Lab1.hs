{- Lab 1
   Date: 2024-11-04
   Authors: Zachris Stenhammar, Elliot Stenhammar
   Lab group: 8
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k+1 


-- B -------------------------
-- power1
power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "power: negative argument"
power1 n k = product [n | x <- [1..k]]

-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k 
 |  even k    =  power2 (n*n) (k `div` 2)
 |  otherwise =  n*power2 n (k-1)


-- D -------------------------


{- 



<Describe your test cases here>
n = 0, k = 4 //testing n=0
n = 1, k = 0 //testing extreme case k=0
n = 5, k = 5  //testing regular use
n = -3, k = 3 //testing negative n
(n = 1, k = -1 //should not work
n = -4, k = -4 //should not work) it was unclear if we were supposed to test negative numbers
 -}

-- 
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = 
   power n k == power1 n k && power1 n k == power2 n k



--
powerTest :: Bool
powerTest = 
   and [prop_powers n k | (n, k) <- [(0,4),(1,0),(5,5),(-3,3)]]

--
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k 
 | k < 0 = True
prop_powers' n k = 
    power n k == power1 n k && power1 n k == power2 n k