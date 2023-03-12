module Demo where

import Data.Char

sumSquares x y = x ^ 2 + y ^ 2
lenVec3 x y z = sqrt ( x ^ 2 + y ^ 2 + z ^ 2 )
fortyTwo = 39 + 3

f x = if x > 0 then 1 else (-1)

g x = ( if x > 0 then 1 else (-1) ) + 3

sign x = if x == 0 then 0 else ( if x > 0 then 1  else (-1) )

max5 x = max 5 x
max6' = max 6
--ассациативность правая 5 приоретета 
-- infixr 5
--ассациативность левая 4 приоретета 
-- infixl 4
-- собственный оператор ассациативность левая 6 приоретета 

infixl 6 *+*
a *+* b = a ^ 2 + b ^ 2
-- (*+*) a b = a ^ 2 + b ^ 2

-- оператор |-|, который возвращает модуль разности переданных ему аргументов:
x |-| y = if x >= y then (-) x y else (-) y x

-- оператор применения функции f  к аргументу x с самым низким приорететом и правой ассациативностью 
f $ x = f x

-- logBase 4 (min 20 (9 + 7)) -> logBase 4 (min 20 $ 9 + 7) -> logBase 4 $ min 20 $ 9 + 7

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum


standardDiscount = discount 1000 5

test = isDigit '7'

{- Реализуйте функцию twoDigits2Int, которая принимает два символа и возвращает число, 
составленное из этих символов, если оба символа числовые, и 100 в противном случае. 
(Первый символ рассматривается как количество десятков, второй — единиц.) -}


twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isNumber x && isNumber y then digitToInt x * 10 + digitToInt y else 100

--примеры кортежа 
--(2, True)
--(2, True, 'c')

{- Будем задавать точки на плоскости парами типа (Double, Double). Реализуйте функцию dist, 
 возвращает расстояние между двумя точками, передаваемыми ей в качестве аргументов.-}
 
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ( (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)
--dist p1 p2 = sqrt $ (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2

-- примеры списков

--[1,2,3]
--['H','i'] тоже самое "Hi"

--ghci> 'H' : "ello"
--"Hello"

--ghci> "Hello" ++ " world"
--"Hello world"

factorial n = if n == 0 then 1 else n * factorial (n-1)

factorial' 0 = 1
factorial' n = n * factorial' (n-1)

{- Определите функцию, вычисляющую двойной факториал, то есть произведение
 натуральных чисел, не превосходящих заданного числа и имеющих ту же четность.-}
 
doubleFact' :: Integer -> Integer
doubleFact' 0 = 1
doubleFact' 1 = 1
doubleFact' n = n * doubleFact' (n-2)

doubleFact :: Integer -> Integer
doubleFact n = if n > 1 then n * doubleFact (n-2) else 1 


factorial'' 0 = 1 

-- охранные выражения: 
factorial''' 0 = 1
factorial''' n | n < 0 = error "arg must be >= 0"
               | n > 0 = n * factorial''' (n-1)


-- последовательность Фибоначчи 
fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' n | n > 0 = fibonacci' (n - 1) + fibonacci' (n - 2)
             | n < 0 = fibonacci' (n + 2) - fibonacci' (n + 1)



factorial5 n | n >= 0    = helper 1 n
             | otherwise = error "arg must be >= 0"

helper acc 0 = acc 
helper acc n = helper (acc * n) (n - 1)


-- последовательность Фибоначчи  с вспомогательной функцией 
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 1 = fhelper 0 1 n
            -- | n < 0 = (-1)^( (-n) + 1 ) * fhelper 0 1 (-n) -- из свойств
            | n < 0 = fminushelper 0 1 n

fhelper a b 1 = 1 * b
fhelper a b n = fhelper b (a + b) (n - 1)
fminushelper a b 1 = 1 * b
fminushelper a b n = fminushelper (b-a) a  (n+1)

roots a b c =
  (
    (-b - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  ,
    (-b + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
  )
  
roots' a b c =
    let d = sqrt (b ^ 2 - 4 * a * c) in
    ((-b - d) / (2 * a), (-b + d) / (2 * a))
	
roots'' a b c =
    let {d = sqrt (b ^ 2 - 4 * a * c); x1 = (-b - d) / (2 * a); x2 = (-b + d) / (2 * a)}
	in (x1, x2)
	
roots''' a b c =
    let 
	  x1 = (-b - d) / aTwice
	  x2 = (-b + d) / aTwice
	  d = sqrt ( b ^ 2 - 4 * a * c )
	  aTwice = 2 * a
	in (x1, x2)

--factorial6 n 
-- | n >= 0 = let
--       helper' acc 0 = acc
--	   helper' acc n = helper' ( acc * n ) (n - 1)
--	in helper' 1 n
--  | otherwise = error "arg must be >= 0"
  
rootsDiff a b c = let
  (x1, x2) = roots a b c 
  in x2 - x1
{- Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности 
a_0 = 1; a_1 = 2; a_2 = 3
a_k+3 = a_k+2 + a_k+1 - 2 * a_k -}  
seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n | n > 2 = ahelper 0 0 1 n
            -- | n < 0 = (-1)^( (-n) + 1 ) * fhelper 0 1 (-n) -- из свойств
			
ahelper n1 n2 n3 2  = n1 * 1 + n2 * 2  + n3 * 3
ahelper n1 n2 n3 n  = ahelper ((-2) * n3) (n3 + n1) (n3 + n2) (n - 1)

roots'''' a b c = (x1, x2) where
	x1 = (-b - d) / aTwice
	x2 = (-b + d) / aTwice
	d = sqrt ( b ^ 2 - 4 * a * c )
	aTwice = 2 * a
	
{- Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа. -}	
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x | x > 0 = shelper 0 0 x   
              | x < 0 = shelper 0 0 (-x)  


shelper sums counts 0 = (sums , counts)            
shelper sums counts x = shelper (sums + (mod x 10)) (counts + 1) (div x 10)


integration :: (Double -> Double) -> Double -> Double -> Double 
integration f a b = integral where
    integral = sumf * d
    sumf = fhelper 1 0 
    fhelper 1000 fi = fi + ((f a) + (f b)) / 2
    fhelper i fi = fhelper (i + 1) (fi + (f (a + d * i)))
    n = 1000
    d = (b - a) / n
    
	 
-- n=1000
-- ihelper f a b n integral = integral * ()
-- ihelper f a b i integral= ihelper f a b (i+1) (integral + (f (a + i * d ) + f (a + (i +1 )* d)) * d / 2) where
-- d=(b - a) / n
	
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageGork a && doesEnrageMork a == 1 = stomp . stab a
                  | doesEnrageGork a == 1 = stab a
                  | doesEnrageMork a == 1 = stomp a
                  | doesEnrageGork a || doesEnrageMork a == 0 = a
