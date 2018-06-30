-- Exercise 4.1

> data Nat = Null | N Nat

> instance Show Nat where
>	show Null = "0"
>	show (N Null) = "1"

>	show n = show ( listToInt (  tail (toTwo ( lob n )) ) )
>		where
>			lob :: Nat -> Int
>			lob (N Null) =   1
>			lob (N n) = 1 + lob n 


> toTwo 0 = [0]
> toTwo n
>		| n `mod` 2 == 1 = toTwo (n `div` 2) ++ [1]
>        | n `mod` 2 == 0 = toTwo (n `div` 2) ++ [0]

> listToInt :: [Int] -> Int
> listToInt xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))

> instance Eq Nat where
>	a == b = ((change a) == (change b ))
>	a /= b = not ((change a) == (change b))

> change :: Nat -> Int
> change Null=0
> change (N Null) =   1
> change (N n) = 1 + change n 

> change2 :: Int -> Nat
> change2 0 = Null
> change2 1 =  (N Null)
> change2 2 =  (N(N Null))
> change2 n =  N (change2 (n-1))

> instance Ord Nat where 
>  a <= b = change a <= change b 
>  b <= a =change b <= change a


> instance Num Nat where 
>	negate x = Null
>	x + y = change2 ((change x) + (change y )) 
>	x - y = change2 ((change x ) - (change y )) 
>	x * y = change2 ((change x ) * (change y )) 
>	fromInteger a = (change2 (fromIntegral a))



> instance Enum Nat where
>	fromEnum Null = 0
>	fromEnum (N n) = 1 + fromEnum n
>	toEnum n
>		| n == 0 = Null
>		| n > 0 = N $ toEnum (n - 1)




-- Exercise 4.2

> type Wahrheitswert = Bool

> data Name = N1 | N2 | N3 | N4 | N5 deriving (Eq,Ord,Enum,Show)

> newtype Variable = Var Name deriving (Eq,Ord,Show)

> instance Enum Variable where
>  fromEnum (Var name) = fromEnum name
>  toEnum n =Var ( toEnum n :: Name)

> data Ausdruck = K Wahrheitswert
>		| V Variable
>		| Nicht Ausdruck
>		| Und Ausdruck Ausdruck
>		| Oder Ausdruck Ausdruck
>		deriving (Eq,Show)
> type Belegung = Variable -> Wahrheitswert

> auswerten :: Ausdruck -> Belegung -> Wahrheitswert
> auswerten (K True)  f = True
> auswerten (K False) f = False
> auswerten (V var1) f = f var1
> auswerten (Nicht ausdruck) f  = not $ auswerten ausdruck f
> auswerten (Oder ausdruck1 ausdruck2) f = (auswerten ausdruck1 f) || (auswerten ausdruck2 f) 
> auswerten (Und ausdruck1 ausdruck2) f = (auswerten ausdruck1 f) && (auswerten ausdruck2 f)

tests:

> secondTest :: Belegung
> secondTest (Var N1) = True
> secondTest (Var N2) = False
> secondTest (Var N3) = True
> secondTest (Var N4) = False
> secondTest (Var N5) = True

> firstTest1 = K True :: Ausdruck
> firstTest2 = K False :: Ausdruck
> firstTest3 = Nicht firstTest1 :: Ausdruck 
> firstTest4 = Nicht firstTest3 :: Ausdruck 
> firstTest5 = Und firstTest1 firstTest3 :: Ausdruck 
> firstTest6 = Und firstTest1 firstTest4 :: Ausdruck 
> firstTest7 = Oder firstTest1 firstTest3 :: Ausdruck 
> firstTest8 = Oder firstTest6 firstTest4 :: Ausdruck 






