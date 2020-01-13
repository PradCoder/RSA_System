module RSA where
import Data.Char
import Data.List.Split

euclidean :: Integer -> Integer -> Integer
euclidean 0 0 = error "GCD(0,0) is undefined"
euclidean 0 b = b
euclidean a b = euclidean (b `mod` a) a

--https://stackoverflow.com/questions/33326716/modular-inverse-in-haskell
exteuclidean :: Integer -> Integer -> (Integer, Integer, Integer)
exteuclidean 0 0 = error "extGCD(0,0) is undefined"
exteuclidean a 0 = (1, 0, a)
exteuclidean a b = 
    let (q, r) = a `quotRem` b
        (c,x,y) = exteuclidean b r
    in (x, c-q*x, y)

-- https://rosettacode.org/wiki/Modular_inverse#Haskell
modInverse :: Integer -> Integer -> Integer
modInverse a m
    | 1 == g = (mkPos i m)
    | otherwise = error "inverse not defined"
    where (i, _, g) = exteuclidean a m

-- https://rosettacode.org/wiki/Modular_inverse#Haskell
mkPos :: Integer -> Integer -> Integer
mkPos x m
    | x < 0 = x + m
    | otherwise = x

fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base 1 m = mod base m
fastPow base pow m | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
                    | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m
                                
stoInteger :: [Char] -> [Integer]
stoInteger xs = [ x | y<-xs , let x = toInteger(ord(y))+100]

sfromInteger :: [Integer] -> [Char]
sfromInteger xs = [ x | y<-xs , let x = chr(fromIntegral(y)-100)]

integerListToString :: [Integer] ->String
integerListToString [] = []
integerListToString (x:xs) = show x ++ integerListToString(xs)

catNumber :: [Integer] -> Integer
catNumber xs = read (integerListToString xs)

revFunction :: Integer -> [[Char]]
revFunction int = let str = show int
                  in chunked str

chunked :: [Char] -> [[Char]]
chunked xs = chunksOf 3 xs

combine :: [[Char]] -> [Integer]
combine xs = map read xs

totalRevFunction :: Integer -> [Char]
totalRevFunction xs = sfromInteger (combine (revFunction xs))

lambdan :: Integer -> Integer -> Integer
lambdan p q = lcm (p-1) (q-1) 
    where lcm p q = (p*q) `div` (euclidean p q) -- lcm = GCD(p,q)

decrypt :: Integer -> Integer -> Integer -> Integer -> [Char]
decrypt cipher d p q = totalRevFunction (fastPow cipher d (p * q))

decryptn :: Integer -> Integer -> Integer  -> [Char]
decryptn cipher d n = totalRevFunction (fastPow cipher d n)

bigPrimes :: (Integer,Integer,Integer) -> [Char]
bigPrimes (c,e,n) = decryptn c (modInverse e n) n

-- The e here is supposed to change
--The small primes for the test n = 101 * 103, c = 125,  e = 13
-- n = p * q, p = 101, q = 103


testPrimeFunction :: Integer -> Integer -> Integer -> Integer-> [Char]
testPrimeFunction message p q e = totalRevFunction (fastPow message (modInverse e (lambdan p q)) (p * q)) -- This thing works for decrypting an encrypted message

-- TODO : Debugged stuff
-- factorial :: (Integral a) => a -> a
-- factorial 0 = 1
-- factorial n = n * factorial(n-1) 

-- wilsons:: Integer -> Integer
-- wilsons n = ((factorial(n) `mod` (n+1)) `div` n)*(n-1)+2

-- c, e, n
-- bigPrimes (2512924383434167076307547317098002231594760350116503932064890779583240111734403324875636824195394184043293688267129793025340634695426766953547128529284711869534989948355405260258003165864709778179451507876975105659068960939586992971282120476547053973122335196039015572503589485191791429360698516077641883247809945119360650682408800757015713996385940670277059670436631898914631366063245474682475393498974608662766769837336842550383697840940,131382739827492849287498274982749872947987241,2662342947009362750634457485784195929685949192026210146800497533981684973874876471715438191927612428468072459721782773957968550246488873788870288718081674045071993376268199513285315649715256148038825550533666880925666906421588476273565780966620801353069399982056196648376443549867153314383231341639629209009642152704560749955512476070844619227546053497347075202732043473493109102249370688400362592524610630473096168534092067476939232340457)

-- If I have p and q I have everything
-- I generate n and d, e n 
-- The decryptor has the information to 


-- Steve's stuff so far
-- Ascii to Integer
-- Encrypt Function with a given e and n

-- My Stuff
-- Integer to Ascii
-- Decryption Function
-- modular exponentiation
-- Prime Generator "The Primes should be of same length in binary for message length"
-- Randomized Padding that adjust the bit-length (This is for taking care of Attacks)
-- Charmichael's Totient Function

-- Extra Stuff
-- Random Number Generator
-- Arithmoi 