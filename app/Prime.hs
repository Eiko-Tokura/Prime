module Prime where

import System.Random
import Conduit

data Z modules = Z { toIntegerZ :: Integer, toModulesZ :: Integer } deriving (Show, Eq, Read)

data PublicKey = PublicKey {publicE :: Integer, publicN :: Integer} deriving (Show, Eq, Ord, Read)
data PrivateKey = PrivateKey {privateE :: Integer, privateN :: Integer, privateD :: Integer} deriving (Show, Read, Ord, Eq)

mod' k m = if k >= m || k < 0 then mod k m else k

instance Num (Z modules) where
  Z a m + Z b _ = Z (mod' (a + b) m) m
  Z a m * Z b _ = Z (mod' (a * b) m) m
  abs (Z a m) = Z (abs a) (abs m)
  negate (Z a m) = Z (negate a) m
  signum (Z a m)  
    | a >  0 = Z 1    m
    | a == 0 = Z 0    m
    | a <  0 = Z (-1) m
  fromInteger n = Z n 1

expZ :: Z m -> Integer -> Z m
expZ (Z a m) n
  | n == 0    = Z (if a == 0 then 0 else 1) m
  | n == 1    = Z a m
  | even n    = let n' = n `div` 2 in expZ (Z a m) n' ^2
  | otherwise = let n' = n `div` 2 in expZ (Z a m) n' ^2 * Z a m

fermat :: Integer -> Integer -> Bool
fermat a p 
  | fermatTest == 1 = True
  | otherwise       = False
    where fermatTest = toIntegerZ $ expZ (Z a p) (p-1)

notDivBy :: Integer -> Integer -> Bool
notDivBy d n
  | n `mod` d == 0 && n > d = False
  | otherwise               = True

testPipes test list = foldl (.|) (filterC . test $ head list) (map (filterC . test) (tail list))

genPrimes primelist range = runConduitPure 
  $  yieldMany [2 .. range]
  .| testPipes notDivBy primelist
  .| sinkList

primes1e1 = [2,3,5,7]
primes1e2 = genPrimes primes1e1 100
primes1e4 = genPrimes primes1e2 10000

getPrimeIO :: (Integer, Integer) -> IO Integer
getPrimeIO (startInt, endInt) = do
  st <- randomRIO (startInt, endInt)
  Just p <- runConduit
    $  yieldMany [st ..]  
    .| testPipes fermat   primes1e2
    .| testPipes notDivBy primes1e4
    .| await
  return p

invZ :: Z m -> Maybe (Z m)
invZ (Z a m) = if d == 1 then Just (Z (x `mod` m) m) else Nothing
  where (x,y,d) = bezout a m

-- input a,b, give x,y,d such that ax+by=d
bezout :: Integer -> Integer -> (Integer, Integer, Integer)
bezout r0 r1 
  | r0 < r1   = (\(x,y,d) -> (y,x,d)) $ bezout r1 r0
  | otherwise = reconstructXYD (1, 0) . (\(l, d) -> (reverse l, d)) $ getListOfQnAndD r0 r1
  where 
    getListOfQnAndD :: Integer -> Integer -> ([Integer], Integer)
    getListOfQnAndD d  0  = ([], d)
    getListOfQnAndD r0 r1 = (q1 : rest, d)
      where q1 = r0 `div` r1
            r2 = r0 `mod` r1
            (rest, d) = getListOfQnAndD r1 r2
    reconstructXYD (xn, yn) (qn:rest, d) = reconstructXYD (yn, xn - yn * qn) (rest, d)
    reconstructXYD (x, y) ([], d) = (x, y, d)

genKey :: Integer -> IO PrivateKey
genKey strength = do
  let strength' = strength + 50
  p <- getPrimeIO (10^strength , 10^strength' ) 
  q <- getPrimeIO (10^strength , 10^strength' ) 
  let m = (p-1) * (q-1)
      n = p * q
  Just (e, d) <- runConduit 
    $  yieldMany [1..]
    .| mapMC (\_ -> randomRIO (10^10, 10^30))
    .| mapC (\e -> (e, bezout e m))
    .| filterC (\(e, (x, y, d)) -> d == 1)
    .| mapC (\(e, (x, y, d)) -> (e, x `mod` m))
    .| await
  putStrLn $ "d = " ++ show d 
  putStrLn $ "e = " ++ show e 
  putStrLn $ "n = " ++ show n 
  let testm = 1234567890123456789 `Z` n
      teste = testm `expZ` e
      testd = teste `expZ` d
  if testm == testd
  then return $ PrivateKey e n d
  else genKey strength 

genKeyPair s = do
  priKey <- genKey s
  return (priKey, privateToPublic priKey)

privateToPublic :: PrivateKey -> PublicKey
privateToPublic (PrivateKey e n _) = PublicKey e n 

information n b = fromIntegral $ information' n b 1
  where information' n b d
          | d < n && d' >= n  = 0
          | otherwise         = 1 + information' n b d'
            where d' = b * d

