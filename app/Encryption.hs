module Encryption where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (isSuffixOf)
import Data.Array.Unboxed

import Conduit
import Data.Word (Word8)
import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Prime

import Control.Parallel
import Control.Parallel.Strategies (rpar, runEval)
import Control.DeepSeq

--testK = read "PrivateKey {privateE = 400294371346487234205413790401, privateN = 31180215529261435474151714301568148382831518939921831025200801904327611535912693353524661567396056483358689259299222697793194839071901924890558429321391704231563090268401393073059172656308683530086673400545875990500172613086759396100631490827776223757376767733698584523609321981775782629942687557081622532056162775502199500353689373045869875685976274291510367419965387045014337203741713242791524252056483960553692038082197817718690245329571698843036340027998836362780433714424540710681277121837147949, privateD = 14035745992815882608473517749616739260768593269977452714601075458131745723976174668173894461715984065369671299906992188959601888669752554426997802506947344986050558438685992160367234779409619072944827945409914887483716346481568663282479112083103365380096562259531992196755105170143281155943637336846877479856768257037243267866658508683152874658397583693557438238414194478281235453157728572726564024551351879342214665947099156843289122501999812624066641693633976772020149797946760402594762893543448961}" :: PrivateKey
--testPub = privateToPublic testK
--
--test = do
--  let testset = "hello owo world"
--      sizeF = information (privateN testK) lenclist + 1
--      int = concatMap (integerToStringFile sizeF) . encryptS testPub $ testset--stringToIntegerFile testset 
--      str = decryptS testK . map stringToIntegerFile . splitPieces sizeF . filter (`elem` clist) $ int
--  print testset
--  print int
--  print str
      --inter = map (integerToChunkFile (sizeF+1)) . encryptW8 (privateToPublic testK) $ testset
  --BS.writeFile "t0" $ BS.pack . concat . map (integerToChunkFile (sizeF+1)) . encryptW8 (privateToPublic testK) $ testset
  ----print inter
  --y <- BS.readFile "t0"
  --let inter2 = decryptW8 testK . map (chunkToIntegerFile ) $ inter
  ----print $ map (chunkToInteger . (\n -> [n,n])) testset
  ----print $ map (integerToChunkExact . chunkToInteger . (\n -> [n,n])) testset
  --print $ decryptW8 testK . encryptW8 (privateToPublic testK) $ testset
  --print "hi"
  --print $ integerToChunkFile sizeF . chunkToIntegerFile $ take sizeF testset
  --print inter2
  --print (inter2 == testset)
  --let testres = decryptW8 testK . map chunkToIntegerFile . splitPieces (sizeF+1) . BS.unpack $ y
  --print testres
  --print $ length testres

chunkToInteger :: [Word8] -> Integer
chunkToInteger listW8 = runConduitPure
  $   yieldMany listW8
  .|  foldlC (\acc x -> 257 * acc + fromIntegral x + 1) 0 -- acc `shiftL` 8 .|. fromIntegral x) 0 

chunkToIntegerFile :: [Word8] -> Integer
chunkToIntegerFile listW8 = runConduitPure
  $   yieldMany listW8
  .|  foldlC (\acc x -> acc `shiftL` 8 .|. fromIntegral x) 0 -- acc `shiftL` 8 .|. fromIntegral x) 0 

integerToChunkFile :: Int -> Integer -> [Word8]
integerToChunkFile size int = (\(x, y) ->  replicate (size - x) 0 ++ reverse y ) $ intToChunk' int
  where intToChunk' 0 = (0, [])
        intToChunk' n = (restSize + 1, fromIntegral (n .&. 0xff) : restChunk)
          where (restSize, restChunk) = intToChunk' (n `shiftR` 8)

integerToChunk :: Int -> Integer -> [Word8]
integerToChunk size int = (\(x, y) ->  replicate (size - x) 0 ++ reverse y ) $ intToChunk' int
  where intToChunk' 0 = (0, [])
        intToChunk' n = (restSize + 1, fromIntegral ((n-1) `mod` 257) : restChunk)
          where (restSize, restChunk) = intToChunk' (n `div` 257)

integerToChunkExact :: Integer -> [Word8]
integerToChunkExact int = reverse $ intToChunk' int
  where intToChunk' 0 = []
        intToChunk' n = fromIntegral ((n-1) `mod` 257) : restChunk
          where restChunk = intToChunk' (n `div` 257)

clist = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']
lenclist = fromIntegral $ length clist
ubclist = fromIntegral $ lenclist - 1

intToCharArray :: UArray Int Char
intToCharArray = array (0, ubclist) (zip [0..ubclist] clist)

charToIntArray :: UArray Char Int
charToIntArray = array ('0', 'z') (zip clist [0..ubclist])

integerToStringFile :: Int -> Integer -> String
integerToStringFile size int = (\(x, y) ->  replicate (size - x) '#' ++ reverse y ) $ intToChunk' int
  where intToChunk' 0 = (0, [])
        intToChunk' n = (restSize + 1, intToCharArray ! fromIntegral (n `mod` lenclist) : restChunk)
          where (restSize, restChunk) = intToChunk' (n `div` lenclist)

stringToIntegerFile :: String -> Integer
stringToIntegerFile listW8 = runConduitPure
  $   yieldMany listW8
  .|  foldlC (\acc x -> acc * lenclist + fromIntegral (charToIntArray ! x)) 0 -- acc `shiftL` 8 .|. fromIntegral x) 0 

integerToStringExact :: Integer -> String
integerToStringExact int = reverse $ intToChunk' int
  where intToChunk' 0 = []
        intToChunk' n = intToCharArray ! ((fromIntegral n - 1) `mod` (fromIntegral lenclist+1)) : restChunk
          where  restChunk = intToChunk' (n `div` (lenclist+1))

stringToInteger :: String -> Integer
stringToInteger str = runConduitPure
  $   yieldMany str
  .|  foldlC (\acc x -> (lenclist+1) * acc + fromIntegral ( charToIntArray ! x) + 1) 0 

encryptOneChunk pub chunk = toIntegerZ $ Z (chunkToInteger chunk) (publicN pub) `expZ` publicE pub

encryptW8Par :: Int -> PublicKey -> [[Word8]] -> [Integer]
encryptW8Par n pub [] = []
encryptW8Par n pub listw8 
  | n <= 20 = runConduitPure $ yieldMany listw8 .| mapC (encryptOneChunk pub) .| sinkList 
  | even n    = runEval $ do
      let (part1, part2) = splitAt (n `div` 2) listw8
      w1 <- rpar . force . encryptW8Par (n `div` 2) pub $ part1
      w2 <- rpar . force . encryptW8Par (n `div` 2) pub $ part2
      return (w1 ++ w2) 
  | otherwise = runEval $ do
      let (part1, part2) = splitAt (n `div` 2) listw8
      w1 <- rpar . force . encryptW8Par (n `div` 2)     pub $ part1
      w2 <- rpar . force . encryptW8Par (n `div` 2 + 1) pub $ part2
      return (w1 ++ w2) 
  where chunkSize = information (publicN pub) 257

encryptW8' :: PublicKey -> [Word8] -> [Integer]
encryptW8' pub = (\l -> encryptW8Par (length l) pub l) . splitPieces chunkSize
  where chunkSize = information (publicN pub) 257

encryptW8 :: PublicKey -> [Word8] -> [Integer]
encryptW8 pub [] = []
encryptW8 pub listw8 = encryptedInt0 : rest
  where (chunk0, chunkRest) = splitAt chunkSize listw8
        encryptedInt0 = encryptOneChunk pub chunk0
        encryptOneChunk pub chunk = toIntegerZ $ Z (chunkToInteger chunk) (publicN pub) `expZ` publicE pub
        rest = encryptW8 pub chunkRest
        chunkSize = information (publicN pub) 257

decryptW8Par :: Int -> PrivateKey -> [Integer] -> [Word8]
decryptW8Par n pri lints
  | n <= 10   = concatMap (integerToChunkExact . decryptInt) lints-- once used non-exact ver
  | even n    = runEval $ do
      let (part1, part2) = splitAt (n `div` 2) lints
      w1 <- rpar . force . decryptW8Par (n `div` 2) pri $ part1
      w2 <- rpar . force . decryptW8Par (n `div` 2) pri $ part2
      return (w1 ++ w2) 
  | otherwise = runEval $ do
      let (part1, part2) = splitAt (n `div` 2) lints
      w1 <- rpar . force . decryptW8Par (n `div` 2) pri $ part1
      w2 <- rpar . force . decryptW8Par (n `div` 2 + 1) pri $ part2
      return (w1 ++ w2) 
  where decryptInt int = toIntegerZ $ Z int (privateN pri) `expZ` privateD pri
        chunkSize = information (privateN pri) 257

decryptW8' pri list = decryptW8Par (length list) pri list

decryptW8 :: PrivateKey -> [Integer] -> [Word8]
decryptW8 pri = concatMap (integerToChunkExact . decryptInt) -- once used non-exact ver
  where decryptInt int = toIntegerZ $ Z int (privateN pri) `expZ` privateD pri
        chunkSize = information (privateN pri) 257

encryptBS :: PublicKey -> BS.ByteString -> [Integer]
encryptBS pub = encryptW8' pub . BS.unpack

encryptT :: PublicKey -> T.Text -> [Integer]
encryptT pub = encryptW8' pub . BS.unpack . TE.encodeUtf8

encryptS pub = encryptT pub . T.pack

encryptS2S pub = concatMap (integerToStringFile size) . encryptS pub
  where size = information (publicN pub) lenclist + 1

decryptS2S pri = decryptS pri . map stringToIntegerFile . splitPieces size . filter (`elem` clist)
  where size = information (privateN pri) lenclist + 1

decryptBS :: PrivateKey -> [Integer] -> BS.ByteString 
decryptBS pri = BS.pack . decryptW8' pri 

--decryptBSPar :: PrivateKey -> Int -> [Integer] -> BS.ByteString 
--decryptBSPar pri nints lints
--  | nints <= 4 = BS.pack . decryptW8 pri $ lints
--  | even nints = runEval $ do
--      let (part1, part2) = splitAt (nints `div` 2) lints
--      bs1 <- rpar . force . decryptBSPar pri (nints `div` 2) $ part1
--      bs2 <- rpar . force . decryptBSPar pri (nints `div` 2) $ part2
--      return (bs1 
--  | otherwise  = runEval $ do
--      let (part1, part2) = splitAt (nints `div` 2) lints
--      bs1 <- rpar . force . decryptBSPar pri (nints `div` 2) $ part1
--      bs2 <- rpar . force . decryptBSPar pri (nints `div` 2 + 1) $ part2

decryptT :: PrivateKey -> [Integer] -> T.Text 
decryptT pri = TE.decodeUtf8 . BS.pack . decryptW8' pri

decryptS pri = T.unpack . decryptT pri

encryptBS2BS pub = BS.pack . concatMap (integerToChunkFile chunkSize') . encryptBS pub -- once used size + 1
  where chunkSize' = information (publicN pub) 256 + 1

encryptFile pub filepath = do
  file <- BS.readFile filepath
  let enFile = encryptBS2BS pub file
  BS.writeFile (filepath ++ ".encrypt") enFile
  --BS.writeFile (filepath ++ ".control") file

splitPieces size [] = []
splitPieces size datas = part0 : rest 
  where (part0, part1ToN) = splitAt size datas
        rest = splitPieces size part1ToN

decryptBS2BS pri = decryptBS pri . map chunkToIntegerFile . splitPieces chunkSize' . BS.unpack
  where chunkSize' = information (privateN pri) 256 + 1

decryptFile pri filepath = do
  file <- BS.readFile filepath
  let deFile = decryptBS2BS pri file
  BS.writeFile (nameChange filepath) deFile
  where nameChange filepath = if ".encrypt" `isSuffixOf` filepath 
          then reverse . drop (length ".encrypt") . reverse $ filepath 
          else filepath ++ ".decrypt"
