{--Beispiel 1, es berechnet Wieviele Wettkombinationen gibt es bei Gewinnspielen dieses Typs. --}

type Nat0 = Integer
type Nat1 = Integer
type GesamtKugelZahl = Nat1
type GezogeneKugelZahl = Nat1
type Spiel =(GesamtKugelZahl,GezogeneKugelZahl)
type Gluecksspiel =(Spiel, Spiel)

fac :: Nat0 -> Nat1 {--hilf methode fuer Beispiel 1--}
fac n 
 |n==0 = 1 
 |n > 0 = n * fac(n-1)
 
anzahlWettKombis :: Gluecksspiel -> Nat0
anzahlWettKombis (x,y)=hfunc x * hfunc y where
	hfunc (n,k) = (fac n) `div` ((fac k) * fac (n-k ))
	
{--Beispiel 2--}
{--liefert die Funktion fib' diejenige eindeutig bestimmte
Zahl k mit Fib(k) = m als Resultat. sonst,liefert die Funktion fib' den Argumentwert
m als Resultat.--}

fib' ::Nat0->Nat0
fib' m 
 | m==0 = 0 
 | m==1 = 1 
 | fhelp 0 m /= (-1) = fhelp 0 m 
 |otherwise = m
 
 {-- help funktionen fuer beispiel 2 --}
fibolist = 0 : 1 : zipWith (+) fibolist (tail fibolist)

check :: Int -> Integer
check n =fibolist !! n


 {-- help funktion fuer beispiel 2 --}
fhelp :: Integer-> Integer -> Integer
fhelp i n 
 |check (fromIntegral i) == n = i 
 |i<= n  = fhelp (i+1) n 
 |otherwise = -1
 
{--Beispiel 3--}
{--die aufsteigend geordnete Liste
der Werte der Fibonacci-Funktion fur die Werte von 0,1,2,...,n liefert.--}

fibs :: Nat0 -> [Nat0]
fibs 0=[0]
fibs 1=[1]
fibs m = take (fromIntegral m+1) fibolist


{--Beispiel 4--}
{--merge zwei list, nimmt zahlen jeweils von jedem list--}

verflechten :: [Int] -> [Int] -> [Int]
verflechten xs []     = xs
verflechten [] ys     = ys
verflechten (x:xs) (y:ys)
 |length xs==0 =  x :  y : xs++ys 
 |length ys==0 =  x :  y : ys++xs 
 |otherwise    = x :  y : verflechten xs ys
 
