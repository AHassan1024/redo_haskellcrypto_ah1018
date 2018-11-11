module Crypto where

import Data.Char

import Prelude hiding (gcd)

{- 
The advantage of symmetric encryption schemes like AES is that they are efficient 
and we can encrypt data of arbitrary size. The problem is how to share the key. 
The flaw of the RSA is that it is slow and we can only encrypt data of size lower 
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with 
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged 
via the aymmetric RSA. 
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd m n
  | n == 0 = m
  | otherwise = gcd n (mod m n)

--
-- I was attempting to use this following helper function method to return gcd m n, 
-- but recurring errors of mis-matched types forced me to reconsider a recursive 
-- function instead.
--  | m == 0 = n
--  | n == 0 = m
--  | otherwise = x
--		where 
--		divslist :: Int -> [Int]
--		divslist  a
--  	          | a == 1 = 1
--		          | otherwise = [b | b <- [1..a], mod a b == 0]
--		[x : xs] = [b | b <- [1..m], b `elem` divslist m && b `elem` divslist n]
--


phi :: Int -> Int
phi m = length([n | n <- [1..m], gcd m n == 1])

--
-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v) 
-- such that au + bv = d
-- Pre: a, b > 0
-- Unsuccessful attempts at solving this led me to discover that there is a specification for this exercise...
extendedGCD :: Int -> Int -> ((Int, Int), Int)
extendedGCD a b
    | b == 0 = ((1, 0), d)
    | otherwise = ((v', (u' - (q * v'))), d)
    where
        d = gcd a b
        (q, r) = quotRem a b
        ((u', v'), _) = extendedGCD b (mod a b)
        
-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m
    = b
    where
        ((b, b'), d) = extendedGCD a m

-- Calculates (a^k mod m)
-- Pre: k >= 0
modPow :: Int -> Int -> Int -> Int
modPow a k m
    | k == 0    = mod 1 m
    | k == 1    = mod a m
    | even k    = mod ((mod (modPow a' (div k 2) m) m) ^ 2) m
    | otherwise = mod (a' * modPow a' (k - 1) m) m
    where
        a' = mod a m
  
-- Returns the smallest integer that is coprime with phi
-- Pre: phi >= 1
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf phi
    = x
    where 
        x : xs = [m | m <- [2..(phi+1)], gcd m phi == 1]

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q
    = ((e, n), (d, n))
    where 
        n = p * q
        n' = (p - 1) * (q - 1)
        e = smallestCoPrimeOf n'
        d = n' - inverse e n'

-- RSA encryption/decryption; (e, n) is the public key
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt m (e, n) = mod (m ^ e) n


rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d, n) = mod (c ^ d) n


-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt a = ord a - (ord 'a') 

-- Returns the n^th letter
toChar :: Int -> Char
toChar n = chr ((ord 'a') + n)

-- "adds" two letters
add :: Char -> Char -> Char
add a b 
    | ((ord b) + (toInt a)) <= ord 'z' = chr ((ord b) + (toInt a))
    | otherwise                        = chr (((ord b) + (toInt a)) - 26)


-- "substracts" two letters
substract :: Char -> Char -> Char
substract a b
    | a == b = 'a'
    | a > b = chr (ord a - toInt b)
    | a < b = chr ((ord a - toInt b) + 26)

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt key m
    | m == [] = ""
    | otherwise = add key m' : ecbEncrypt key ms
    where
        (m' : ms) = m



ecbDecrypt :: Char -> String -> String
ecbDecrypt key m
    | m == [] = ""
    | otherwise = substract key m' : ecbDecrypt key ms
    where 
        (m' : ms) = m

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--
cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt key iv m = error "TODO: implement cbcEncrypt"

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt key iv m = error "TODO: implement cbcDecrypt"
