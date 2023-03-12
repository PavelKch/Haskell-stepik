module Demo where

import Data.Char
import Data.Function
 -- параметрический полиморфизм
 -- простейший пример 
 -- let id x = x
 -- let k x y = x
 
 {- Напишите функцию трех аргументов getSecondFrom,
 полиморфную по каждому из них, которая полностью игнорирует первый и третий аргумент,
 а возвращает второй. Укажите ее тип. -}
getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom a b c = b

mono :: Char -> Char 
mono x = x 

semiMono :: Char -> a -> Char
semiMono x y = x

-- пример функции высшего порядка 
--apply2 f x = f (f x)
--flip f y x = f x y 

{- В модуле Data.Function определена полезная функция высшего порядка -}
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y
{- Она принимает четыре аргумента: бинарный оператор с однотипными аргументами 
(типа b), функцию f :: a -> b, возвращающую значение типа b, и два значения типа a. 
Функция on применяет f дважды к двум значениям типа a и передает результат в бинарный оператор. -}

{- Используя on можно, например, записать функцию суммирования квадратов аргументов так: -}
--sumSquares = (+) `on` (^2)

{- Функция multSecond, перемножающая вторые элементы пар, реализована следующим образом -}
 
--multSecond = g `on` h

g = (*)
h = (snd)

--анонимные функции 
--let f x =2 * x + 7
--f 10
--27
--(\x -> (2 * x + 7)) 10
--27
--let f' = \x -> 2 * x + 7

-- пример
--let p1 = ((1,2),(3,4))
--let p2 = ((3,4),(5,6))

--fst $ fst p1 -- =1

--subFstFst = (+) `on` helper
--	where helper pp = fst $ fst pp
	
--subFstFst' = (+) `on` (\pp -> fst $ fst)


{- Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую 
в качестве первого аргумента трехместную функцию: -}
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y)  (f z)

-- Композиция 
--subFstFst'' = (+) `on` ( fst . fst)



{-Реализуйте класс типов Printable, предоставляющий один метод toString 
— функцию одной переменной, которая преобразует значение типа, являющегося представителем Printable, 
в строковое представление.

Сделайте типы данных Bool и () представителями этого класса типов, обеспечив следующее поведение: -}

{- GHCi> toString True
"true"
GHCi> toString False
"false"
GHCi> toString ()
"unit type" -}

class Printable a where
    toString :: a -> String
	
instance Printable Bool where 
    toString True = "true"
    toString False = "false"

instance Printable () where 
    toString () = "unit type"

{- Сделайте тип пары представителем класса типов Printable, 
реализованного вами в предыдущей задаче, обеспечив следующее поведение: -}

{- GHCi> toString (False,())
"(false,unit type)"
GHCi> toString (True,False)
"(true,false)" -}

instance (Printable a, Printable b ) => Printable(a,b) where 
    toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
    
-- расширение классов
class (Eq a) => Ord a where 
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min :: a -> a -> a
    compare :: a -> a -> Ordering

-- двумя классами 
class (Eq a, Printable a) => MyClass a where 

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageGork a && doesEnrageMork a == True = stomp (stab a)
                  | doesEnrageGork a == True = stab a
                  | doesEnrageMork a == True = stomp a
                  | doesEnrageGork a || doesEnrageMork a == False = a
				  
class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a| maxBound == a = minBound 
         | otherwise = succ a 
  spred :: a -> a
  spred a| minBound == a = maxBound 
         | otherwise = pred a 
