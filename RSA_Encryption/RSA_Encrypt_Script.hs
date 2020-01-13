module RSA where

import System.Random
import Data.Char


prime_p = 1617715056246752538785299000182419637404531613327842962881457112604034232033671282712980585771232452841883594826372983832472328460350049283645179688250138009533754345217561268639160328774125180445430290589378901663207059
prime_q = 1645742825183467619487091143114742813111578265693364754500359034725510391201898956997827282590265473150210112121750332633893008983436472616485037774050768689235054669072556953131927823038824414275347022148320732189275923
int_e = 131382739827492849287498274982749872947987241

-- returns n
mult_p_q :: Integer -> Integer -> Integer
mult_p_q p q = p * q


totient_p_q :: Integer -> Integer -> Integer
totient_p_q p q = lcm (p - 1) (q - 1)

-- returns d
getGCD :: Integer -> Integer -> Integer
getGCD e totient = if e == 0
                   then totient
                   else if e == 1
                   then 1
                   else getGCD (totient `mod` e) e

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
modInverse e totient
    | 1 == g = mkPos d totient
    | otherwise = error "No modular inverse"
    where (d, _, g) = exteuclidean e totient

-- https://rosettacode.org/wiki/Modular_inverse#Haskell
mkPos :: Integer -> Integer -> Integer
mkPos x m
    | x < 0 = x + m
    | otherwise = x

-- n = pq
fastCiphertext :: Integer -> Integer -> Integer -> Integer
fastCiphertext message exponent n = if exponent == 0 
                               then 1 `mod` n
                               else (let u = (((fastCiphertext message (exponent `div` 2) n) * (fastCiphertext message (exponent `div` 2) n)) `mod` n)
                                     in (if (exponent `mod` 2 == 1)
                                        then ((u*message) `mod` n)
                                        else u) 
                                        ) 

-- VERY SLOW
fastPlaintext :: Integer -> Integer -> Integer -> Integer
fastPlaintext ciphertext d n = if d == 0 
                               then 1 `mod` n
                               else (let u = (((fastPlaintext ciphertext (d `div` 2) n) * (fastPlaintext ciphertext (d `div` 2) n)) `mod` n)
                                     in (if (d `mod` 2 == 1)
                                        then ((u*ciphertext) `mod` n)
                                        else u) 
                                     ) 

-- https://stackoverflow.com/questions/33326716/modular-inverse-in-haskell?fbclid=IwAR21xOcGazPaOv2IWPuHhqavw9Ia0g8m_Io64GPHoG5FjxTWSx5b5GQ_10g
fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base 1 m = mod base m
fastPow base pow m | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
                   | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m

random_e :: Integer -> IO Integer
random_e totient = getStdRandom (randomR (2,totient - 1))

integerListToString :: [Integer] -> String
integerListToString [] = []
integerListToString (x:xs) = show x ++ integerListToString xs

stoIntegerList :: [Char] -> [Integer]
stoIntegerList xs = [ x | y<-xs , let x = toInteger(ord(y))+100]

sfromIntegerList :: [Integer] -> [Char]
sfromIntegerList xs = [ x | y<-xs , let x = chr(fromIntegral(y)-100)]


messageConversion :: String -> Integer
messageConversion message = read (integerListToString ((stoIntegerList message)))

-- returns c, e, n
encrypt :: String -> (Integer, Integer, Integer)
encrypt message = let u = prime_p * prime_q in (fastPow (messageConversion message) int_e u, int_e, u)


--fastCiphertext (messageConversion message) e (mult_p_q p q)

--((messageConversion message)^e) `mod` (mult_p_q p q)
                      
                      

