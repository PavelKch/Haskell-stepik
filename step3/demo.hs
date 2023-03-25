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
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs

{- Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом. -}

{-GHCi> isPalindrome "saippuakivikauppias"
True
GHCi> isPalindrome [1]
True
GHCi> isPalindrome [1, 2]
False-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome  x  = reverse x == x


{- Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка должна быть равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.

GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3] -}

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 (x:xs) [] [] = x : sum3 xs [] []
sum3 [] (x:xs) [] = x : sum3 [] xs []
sum3 [] [] (x:xs) = x : sum3 [] [] xs 
sum3 (x:xs) (y:ys) [] = (x + y) : sum3 xs ys []
sum3 [] (x:xs) (y:ys) = (x + y) : sum3 [] xs ys
sum3 (y:ys) [] (x:xs) = (x + y) : sum3 ys [] xs 
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs


{- Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) и возвращает список таких групп.

GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]] -}

groupElems :: Eq a => [a] -> [[a]]
groupElems []     = []
groupElems (x:xs) = gacc xs [x] []
  where
    gacc []     acc     all  = reverse $ acc:all
    gacc (x:xs) (z:acc) all | x == z    = gacc xs (z:z:acc) all
    gacc (x:xs) (z:acc) all | otherwise = gacc xs [x]       ((z:acc):all)
	
--testing :: Eq a => [a] -> ([a], [a])
--testing (x:xs) = if x == head xs then ([x : testing (x:xs)],[]) else ([x],[xs])

{- Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
GHCi> readDigits "365ads"
("365","ads")
GHCi> readDigits "365"
("365","") -}

readDigits :: String -> (String, String)
readDigits xs = span isDigit xs

{- Реализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую список элементов, удовлетворяющих хотя бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11] -}

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 [] = []
filterDisj p1 p2 (x:xs)
    | (p1 x) || (p2 x) = x : filterDisj p1 p2 xs
	| otherwise = filterDisj p1 p2 xs
	
{- Напишите реализацию функции qsort. Функция qsort должная принимать на вход список 
элементов и сортировать его в порядке возрастания с помощью сортировки Хоара: для какого-то элемента 
x изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x, и потом 
запускаться рекурсивно на обеих частях.

GHCi> qsort [1,3,2,5]
[1,2,3,5] -}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs)= qsort ( filter ( <= x) xs) ++ [x] ++ qsort ( filter ( > x) xs)

{- Напишите функцию squares'n'cubes, принимающую список чисел,
и возвращающую список квадратов и кубов элементов исходного списка.
GHCi> squares'n'cubes [3,4,5]
[9,27,16,64,25,125] -}

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

{- Воспользовавшись функциями map и concatMap, определите функцию perms,
 которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке.
 
GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]] -}

perms :: [a] -> [[a]]
perms [] = [[]]
perms x = [x]
perms xs@(x:xs') = helper xs ++ map (\x -> reverse x ) (helper xs) where
    helper xs = map (\x -> drop x xs ++ take x xs) [0 .. ((length xs) - 1)]

qwer :: [a] -> [[a]]
qwer [] = []
qwer xs = map (\x -> drop x xs ++ take x xs) [0 .. ((length xs) - 1)]
qwer2 xs = qwer xs ++ map (\x -> reverse x ) (qwer xs)




{- Реализуйте функцию delAllUpper, удаляющую из текста все слова, 
целиком состоящие из символов в верхнем регистре. Предполагается, 
что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.

GHCi> delAllUpper "Abc IS not ABC"
"Abc not" -}

delAllUpper :: String -> String
delAllUpper = unwords . filter (any isLower) . words where 

{- Напишите функцию max3, которой передаются три списка одинаковой длины и 
которая возвращает список той же длины, содержащий на k-ой позиции наибольшее 
значение из величин на этой позиции в списках-аргументах.

GHCi> max3 [7,2,9] [3,6,8] [1,8,10]
[7,8,10]
GHCi> max3 "AXZ" "YDW" "MLK"
"YXZ" -}

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 max3' where 
    max3' x y z = max x (max y z)
	
{- Реализуйте c использованием функции zipWith функцию fibStream, 
возвращающую бесконечный список чисел Фибоначчи.

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34] -}
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)