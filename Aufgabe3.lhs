> import Data.List

Exercise 3.1
primes create list of prime numbers.

> primes = [n | n<-[2..], not $ elem n [j*k | j<-[2..n-1], k<-[2..n-1]]]

x produce multiplication of first three element of a list.

> x :: [Integer] -> Integer
> x list = list !! 0 * list !! 1 * list !! 2  

y create list of tripple prime numbers up to a certain number. 

> y :: [Integer] -> Integer -> [Integer] -> [Integer]
> y (prime:primes) hoechsteZahl list|(x (prime:primes)) > hoechsteZahl = list
>                                   |otherwise = y primes hoechsteZahl (list++[(x (prime:primes))])

getMxL get the maximum of a list.

> getMxL :: [Integer] -> Integer -> Integer
> getMxL [] number = number
> getMxL (x:xs) number
>  | (x>number) = getMxL xs x
>  | otherwise = getMxL xs number

getM get the minimum of a list.

> getM :: [Integer] -> Integer -> Integer
> getM [] number = number
> getM (x:xs) number
>  | (x<number) = getM xs x
>  | otherwise = getM xs number

> type Zahlenliste = [Integer]
> type Tripelprimzahl = Integer

schuerfenList create list of tripple prime numbers based on min and max of a given list.

> schuerfenList :: Zahlenliste -> [Tripelprimzahl]
> schuerfenList numberlist=[n|n<-y(take(fromIntegral(getMxL(numberlist)0))primes)(getMxL numberlist 0)[],n>=(getM(numberlist)((getMxL numberlist 0 )))]

> schuerfen :: Zahlenliste -> [Tripelprimzahl]
> schuerfen x
>	|((length x)==1 ) && (length (prime_factors (head x))==3)= x
>	|((length x)==1 ) && (length (prime_factors (head x))/=3)= []
>	|otherwise= schuerfenCheck x (schuerfenList x) []

> prime_factors :: Integer -> [Integer]
> prime_factors 1 = []
> prime_factors n
>   | factors == []  = [n]
>   | otherwise = factors ++ prime_factors (n `div` (head factors))
>   where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

scan the given list with list of triple prime numbers and give them out if they are triple prime based on the correct order.

> schuerfenCheck :: Zahlenliste -> [Integer] -> [Tripelprimzahl] -> [Tripelprimzahl]
> schuerfenCheck [] primes list = list
> schuerfenCheck (x:xs) primes list
>  | ((any (==x) primes) == True) = schuerfenCheck xs primes (list++[x])
>  | otherwise = schuerfenCheck xs primes (list)

Exercise 3.2
 
> newtype Kurs = K Float deriving (Eq,Ord,Show) 
> instance Num Kurs where
> 	(+)(K a) (K b) = K (a + b)
> 	(*)(K a) (K b) = K (a * b)
>	(-)(K a) (K b) = K (a - b)
>	negate (K a) = K (0-a)
>	signum (K a)
>		| a < 0 = -1
>		| a > 0 = 1
>		| a == 0 =0
> 	abs(K a)
>		| a > 0 = K  a
>		|otherwise = K (negate a)
>	fromInteger a = K (fromInteger a)
> newtype Pegelstand = Pgl Float deriving (Eq,Ord,Show)
> instance Num Pegelstand where
>	(Pgl a) + (Pgl b) = Pgl (a + b)
>	Pgl a - Pgl b = Pgl (a - b)
>	Pgl a * Pgl b = Pgl (a * b)
>	negate (Pgl a) = Pgl (0-a)
> 	abs(Pgl a)
>		| a > 0 = Pgl  a
>		|otherwise = Pgl (negate a)
>	signum (Pgl a)
>		| a < 0 = -1
>		| a > 0 = 1
>		| a == 0 =0
>	fromInteger a = Pgl (fromInteger a)


Exercise 3.3
implementation of different functions.

> curry3 :: (( a,b,c ) -> d ) -> a -> b -> c -> d
> curry3 f a b c = f ( a,b,c )

	b is for test of curry3

 b :: (Integer,Integer,Integer) -> Integer
 b (x,y,z) = (x*y*z)

> uncurry3 :: ( a -> b -> c -> d ) -> ( a,b,c ) -> d
> uncurry3 f ( a,b,c ) = f a b c 

	a is for test of uncurry3

 a :: Integer -> (Integer -> Integer -> Integer) 
 a x y z = (x*y*z)

> curry_flip :: (( a,b ) -> c ) -> ( b -> a -> c )
> curry_flip f a b = f(b,a)

	d is for test of curry_flip

 d :: (Integer,Integer) -> Integer
 d (x,y) = (x*y)

> uncurry_flip :: (a -> b -> c) -> ((b,a) -> c)
> uncurry_flip f (a,b) = f b a

	c is for test of uncurry_flip

 c :: Integer -> (Integer -> Integer) 
 c x y = (x*y)

Exercise 3.4
verflechten3 merge three list together and get each time an element from each list, if a list is empty then we use verflechten for other two.

> verflechten :: [Int] -> [Int] -> [Int]
> verflechten xs []     = xs
> verflechten [] ys     = ys
> verflechten (x:xs) (y:ys)
>  |length xs==0 =  x :  y : xs++ys 
>  |length ys==0 =  x :  y : ys++xs 
>  |otherwise    = x :  y : verflechten xs ys
> verflechten3 :: [Int] -> [Int] -> [Int] -> [Int]
> verflechten3 xs [] []  = xs
> verflechten3 [] ys []  = ys
> verflechten3 [] []  zs = zs
> verflechten3 xs ys []  = verflechten xs ys
> verflechten3 xs [] zs  = verflechten xs zs
> verflechten3 [] ys zs  = verflechten ys zs
> verflechten3 (x:xs) (y:ys) (z:zs)
>  |length xs==0 = x :  y : z:	xs++(verflechten ys zs)  
>  |length ys==0 = x :  y : z:	ys++(verflechten xs zs) 
>  |length zs==0 = x :  y : z:	zs++(verflechten xs ys) 
>  |otherwise    = x :  y : z : verflechten3 xs ys zs
