module HW01 where

creditCardNumber = 3330003030333

--------
-- Ex1
--------
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10

--------
-- Ex2
--------
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n 
	| n > 0 = lastDigit(n) : toDigits (dropLastDigit(n) ) 
	| n < 0 = []

--------
-- Ex3
--------
countList :: [Integer] -> Integer
countList lst =
	if null lst
	then 0
	else 1 + countList(tail lst) 

-- Because the list is reversed.. [because i got stuck] the logic is reversed also..
-- if we get 8765 we turn it into [5,6,7,8] after toDigits
-- so if we stay with the real logic of the double from right to left
-- we get [10,6,14,8] instead of the real doubled values [5,12,7,16]
-- so I shifted the double by one =]

doubleEveryDigit :: [Integer] -> [Integer]
doubleEveryDigit lst = 
	if null lst
	then []
	else
		if (countList lst) `mod` 2 == 0
		then  (head lst) : 2 * head(tail lst) : doubleEveryDigit(tail(tail lst))
		else head lst : doubleEveryDigit (tail lst)

--------
-- Ex4
--------
sumDigitsNum :: Integer -> Integer
sumDigitsNum 0 = 0
sumDigitsNum n =
	if n < 10
	then n
	else (lastDigit n) + sumDigitsNum (dropLastDigit n)


sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits lst = sumDigitsNum (head lst) + sumDigits (tail lst)

--------
-- Ex5
--------

isValidCreditCard :: Integer -> Bool
isValidCreditCard 0 = False
isValidCreditCard n =
	if n `mod` 10 == 0
	then True
	else False 


-- ccn = credit card number
validate :: Integer -> Bool
validate ccn = isValidCreditCard (sumDigits (doubleEveryDigit (toDigits ccn)))
