module Demo where

import Data.Char
import Data.Function
 
 
{- еализуйте функцию addTwoElements, которая бы добавляла два переданных ей 
значения в голову переданного списка. -} 
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a1 a2 [] =  a1 : a2 : []
addTwoElements a1 a2 (x:xs) =  a1 : a2 : (x:xs)

{- Реализуйте функцию nTimes, которая возвращает список, состоящий из повторяющихся 
значений ее первого аргумента. Количество повторов определяется значением 
второго аргумента этой функции. -}
nTimes:: a -> Int -> [a]
nTimes x n = helper n [] where
    helper 0 ans = ans
    helper n ans = helper (n - 1) (x : ans)
	
{- Сформируйте список целых чисел, содержащий только те элементы исходного списка, 
значение которых нечетно. -}

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly xs = helper xs [] where
    helper [] ans = ans
    helper xs ans = if odd (head xs) 
	    then helper (tail xs) ((head xs) : ans)
		else helper (tail xs) (ans)
	
