--Exercise 2.1
type N1 = Int 

--(m,n)=>(p,q), in which: m*p=n*q
p2p ::( N1 , N1 ) -> ( N1 , N1 ) 
p2p (m , n)
 |m == 0 = (m,m)
 |n == 0 = (n,n)
 |otherwise =((n `div`(ggt m n)),(m `div`(ggt m n)))

--ggt calculate the ggt (Greatest common divisor) of 2 numbers
ggt :: Int -> Int -> Int 
ggt m n
 | n == 0 	= m
 | n > 0 	= ggt n (ymod m n )

--ymod calculate the rest of division of two numbers.
ymod :: Int -> Int -> Int
ymod m n
 | m < n = m
 | m >= n = ymod (m - n) n

--Exercise 2.2
type Nat0 = Integer
type Nat1 = Integer
type GesamtKugelZahl = Nat1
type GezogeneKugelZahl = Nat1
type Spiel = (GesamtKugelZahl,GezogeneKugelZahl)
type Gluecksspiel = (Spiel,Spiel)
type AngeboteneSpiele = [Gluecksspiel]
--from last exercise
fac :: Nat0 -> Nat1 
fac n 
 |n==0 = 1 
 |n > 0 = n * fac(n-1)
 ----from last exercise
anzahlWettKombis :: Gluecksspiel -> Nat0
anzahlWettKombis (x,y)=hfunc x * hfunc y where
	hfunc (n,k) = (fac n) `div` ((fac k) * fac (n-k ))

--calculate anzahlwetkombis for all element of the list
anzahlWettKombisList :: AngeboteneSpiele -> [Integer]
anzahlWettKombisList l = [ anzahlWettKombis n | n <-l]

--find the element that hast the max value of anzahlwettkombis
check::AngeboteneSpiele ->  [Gluecksspiel]
check ns = [ m | m <- ns, anzahlWettKombis m == maxElem (anzahlWettKombisList ns)] 

--reverse everything , first i made the list in att and then i needed to reverse it, because the order was wrong.
attraktiveSpieleVorne :: AngeboteneSpiele->  [Gluecksspiel]
attraktiveSpieleVorne g= attraktiveSpieleVorneR (att g)

--given a list, reverse all element of it.
attraktiveSpieleVorneR :: [Gluecksspiel]->  [Gluecksspiel]
attraktiveSpieleVorneR [] = []
attraktiveSpieleVorneR (x:xs) = (attraktiveSpieleVorneR xs) ++ [x]


att :: AngeboteneSpiele ->  [Gluecksspiel]
att [] = []
att ns = [head (check ns)] ++ att (neww (attraktiveSpieleVorneR ns))
--attraktiveSpieleVorne ns = [ m | m <- ns, anzahlWettKombis m == maxElem (anzahlWettKombisList ns)] ++ attraktiveSpieleVorne (neww ns) --*
--attraktiveSpieleVorne ns = [ m | m <- ns, anzahlWettKombis m == minElem (anzahlWettKombisList ns)] ++ attraktiveSpieleVorne (neww ns) 
--attraktiveSpieleVorne ns = [head (check ns)] ++ attraktiveSpieleVorne (neww ( ns))


--make a new list without minimum
neww :: AngeboteneSpiele ->  AngeboteneSpiele
neww xs = filter (\x -> x /= maximum xs) xs--it was maximum before , when it was star

--find max
maxElem :: Ord a => [a] -> a
maxElem [] = error "wrong input"
maxElem (y:[]) = y
maxElem (x:y:ys) = maxElem ((max x y) : ys)

--find min
minElem :: Ord a => [a] -> a
minElem [] = error "wrong input"
minElem (y:[]) = y
minElem (x:y:ys) = minElem ((min x y) : ys)


--Exercise 2.3
type Toepfchen   = [Int]
type Kroepfchen  = [Int]
type Zahlenliste = [Int]
aufteilen :: Zahlenliste -> (Toepfchen,Kroepfchen)

--aufteilen :: Nat -> (Nat,Nat)
aufteilen ns= (gs ns, ss ns)

--calculate good numbers. 
gs ::Nat -> Toepfchen
gs ns=[ n | n <-ns, mod (countOne 1 (toThreeH n ))3 == 0 ]

--calculate bad numbers.
ss ::Nat -> Kroepfchen
ss ns=[ n | n <-ns,  mod (countOne 1 (toThreeH n )) 3 /= 0]

--convert from decimal to base 3.
toThreeH 0 = [0]
toThreeH n
		| n `mod` 3 == 2 = toThreeH (n `div` 3) ++ [2]
		| n `mod` 3 == 1 = toThreeH (n `div` 3) ++ [1]
        | n `mod` 3 == 0 = toThreeH (n `div` 3) ++ [0]

--count number of 1 in a given list
countOne :: Int -> [Int] -> Int
countOne x [] = 0
countOne x (c:cs) | x == c 			= 1 + countOne x cs
                  | otherwise 		= countOne x cs

--Exercise 2.4
type Nat = [Int]
ziffern = [0,1,2,3,4,5,6,7,8,9] :: [Int]

--check if a given list is corect presentation of a number
istGueltig :: Nat -> Bool
istGueltig []=True
istGueltig (x:xs) |x<0 =False
				  |x>9 =False
				  |otherwise = istGueltig xs

--add two list
addiere :: Nat -> Nat -> Nat
addiere a b
	|istGueltig a==False =[]
	|istGueltig b==False =[]
	|otherwise=intToList(addiereHelper a b)

--help for addiere
addiereHelper :: Nat -> Nat -> Int
addiereHelper a b = listToInt a + listToInt b

--listToInt: from list to int
listToInt :: Nat -> Int
listToInt xs = sum (zipWith (*) (reverse xs) (iterate (*10) 1))

--intToList: from int to list
intToList :: Int -> Nat
intToList 0 = [0]
intToList x = intToList (x `div` 10) ++ [x `mod` 10]

--convert a list to normal reprsentation of a number in list form.
normalForm :: Nat -> Nat
normalForm l
 |istGueltig l==False =[]
 |allZero l==True =[0] 
 |(head l)==0	= normalForm(tail l)
 |otherwise		= l

--check if a list if just zeroes
allZero:: Nat -> Bool
allZero []=True
allZero (x:xs) |x/=0 =False
			   |otherwise = allZero xs

--subtrahiere two number, only if they are gueltig, or right representation of a list
subtrahiere :: Nat -> Nat-> Nat
subtrahiere n m
 |istGueltig n==False =[]
 |istGueltig m==False =[]
 |n==[] = []
 |n==[0] = [0]
 |m==[]=[]
 |listToInt n==listToInt m = [0] 
 |otherwise =intToList(sub n m)

 --help for subtrahiere, find the bigger number out of two.
sub :: Nat -> Nat -> Int
sub a b 
 | listToInt a>listToInt b = (listToInt a - listToInt b)
 | otherwise 			   = 0


