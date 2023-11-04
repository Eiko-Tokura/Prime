module Main where

import qualified MonParserF as MP
import qualified Data.Set as S
import Prime
import Encryption
import Contacts
import System.Random
import System.Environment (getArgs)
import System.IO
import Text.Read (readMaybe)
import System.Directory (doesFileExist)
import Control.Monad
import Conduit

privatePath = "privateKey.prikey"
publicPath = "publicKey.pubkey"
myCardPath = ".contact"
contactsPath = "allcontacts.txt"

safeInputStr :: String -> [(String -> Bool, String)] -> IO String
safeInputStr hint filters = do
  putStrLn hint
  x <- getLine 
  let emsgs = [emsg ++ "\n" | (True, emsg) <- map (\(f, str) -> (f x, str)) filters] in case emsgs of
      [] -> return x
      _ -> putStrLn (concat emsgs) >> safeInputStr hint filters

safeInput :: (Read a) => String -> [(a -> Bool, String)] -> IO a
safeInput hint filters = do
  putStrLn hint
  ma <- readMaybe <$> getLine 
  case ma of
    Nothing -> putStrLn "Wrong format." >> safeInput hint filters
    Just x  -> let emsgs = [emsg ++ "\n" | (True, emsg) <- map (\(f, str) -> (f x, str)) filters] in case emsgs of
      [] -> return x
      _ -> putStrLn (concat emsgs) >> safeInput hint filters

maybeInputStr :: String -> IO (Maybe String)
maybeInputStr hint = do
  putStrLn hint
  str <- getLine
  if null str then return Nothing
  else return (Just str)

data SupportedData = Message String | File String deriving (Show)
data Action = Encrypt {target_name :: Maybe String, rawM :: Maybe SupportedData}
            | Decrypt {rawM :: Maybe SupportedData}
            | LoadContact {file_path :: String} 
            | NothingSpecified
            | DisplayHelp
            deriving Show

parseArgs :: MP.ParserF String Action
parseArgs = do
  parseLContact <> parseHelp <> parseEncrypt <> parseDecrypt <> parseNoTargetEncrypt <> parseNothingSpecified
  where
    parseLContact = do
      contactPath <- MP.mRunParserF parseContactName <$> MP.item
      case contactPath of
        Nothing -> MP.zero
        Just path -> return $ LoadContact path
    parseContactName = do
      headStr <- MP.many (MP.non '.')
      MP.just '.'
      MP.string "contact"
      return $ headStr ++ ".contact"
    parseEncrypt :: MP.ParserF String Action
    parseEncrypt = do
      MP.just "-to"
      MP.item >>= (\target -> (do MP.just "-file"; Encrypt (Just target) . Just . File  <$> MP.item)
                  <> 
                  (do MP.just "-string";  Encrypt (Just target) . Just . Message <$> MP.item )
                  <> 
                  (do Encrypt (Just target) . Just . Message <$> MP.item)
                  <> (return $ Encrypt (Just target) Nothing) )
    parseNoTargetEncrypt = do Encrypt Nothing . Just . File  <$> MP.item
    parseDecrypt = 
      do MP.just "-d"; MP.just "-file"; Decrypt . Just . File <$> MP.item
      <>
      do mfp <- MP.mRunParserF parserDecryptFileName <$> MP.item
         maybe MP.zero (return . Decrypt . Just .File) mfp
      <>
      do MP.just "-d"; MP.just "-string"; Decrypt . Just . Message <$> MP.item
      <>
      do MP.just "-d"; Decrypt . Just .  Message <$> MP.item
      <>
      do MP.just "-d"; return $ Decrypt Nothing
    parserDecryptFileName = do
      headStr <- concat <$> MP.many (do s <- MP.many (MP.non '.'); MP.just '.'; return $ s ++ ".")
      MP.string "encrypt" 
      return $ headStr ++ "encrypt"
    parseHelp = do
      MP.just "-h" <> MP.just "-help"
      MP.many0 MP.item
      return DisplayHelp
    parseNothingSpecified = do
      return NothingSpecified

detectAndCreateCTCs = do
      ctcExists <- doesFileExist contactsPath
      unless ctcExists $ writeFile contactsPath (show (S.empty :: S.Set Contact)) 

takeAction :: PrivateKey -> Action -> IO ()
takeAction _ (LoadContact fnm) = do
  contact <- readMaybe <$> readFile fnm
  case contact of
    Nothing -> putStrLn $ "The file " ++ fnm ++ " is in wrong format, cannot import."
    Just ctc -> do 
      putStrLn $ "Successfully imported " ++ fnm ++ " into your contact list. "
      note <- maybeInputStr "Do you want to write a note or remark on this contact? (if you do, what you write here may be used to search for this contact. If not, press Enter and leave blank): "
      let mctc = metaData ctc
          ctc' = ctc {metaData = mctc { memo = note }}
      detectAndCreateCTCs
      allctc <- withFile contactsPath ReadMode $ \handle -> do
        contents <- hGetContents handle
        -- Make sure to force the contents to be read while the file is open
        length contents `seq` return (read contents :: S.Set Contact)
      let allctc' = S.insert ctc' allctc
      -- writeFile contactsPath (show allctc')
      withFile contactsPath WriteMode $ \handle -> do
        hPutStr handle (show allctc')
      putStrLn "contact update complete."

takeAction _ (Encrypt (Just target) (Just enObj)) = do
  fctc <- S.filter (\ctc -> name ctc == target || (memo . metaData) ctc == Just target) <$> readAllContacts contactsPath 
  let lctc = S.toList fctc
      size = S.size fctc 
  if size == 0 then putStrLn $ "Username '" ++ target ++ "' does not exist in your contact list."
  else if size > 1 
  then do
    putStrLn $ "There are more than one users named " ++ target ++ " in your contact list. You can consider fix this later, please specify which one for now: " ++ show (zip [0..] $ map (\c -> (name c, metaData c)) lctc)
    idnumber <- safeInput ("Input the id (0~" ++ show (size -1) ++ "): ") [((<0), "cannot be negative."), ((>= size ), "too big, out of range.")]
    encryptTo (lctc !! idnumber) enObj
  else do
    encryptTo (head lctc) enObj

takeAction p (Encrypt Nothing (Just enObj)) = do
  putStrLn $ "it seems that you want to encrypt " ++ show enObj
  target <- safeInputStr "Please specify who do you want to encrypt for: " [(null, "Cannot be empty")]
  takeAction p (Encrypt (Just target) (Just enObj))

takeAction p (Encrypt (Just target) Nothing) = do
  putStrLn $ "it seems that you want to send encrypted messages to  " ++ show target
  str <- putStrLn "Please input the message do you want to encrypt for (multiline, input a single line with '.' to stop) " >> multiLine 
  takeAction p (Encrypt (Just target) (Just . Message $ str))
  where 
    multiLine = do
      line0 <- getLine
      if line0 == "." then return "" else ((line0 ++ "\n") ++) <$> multiLine

takeAction priKey (Decrypt Nothing) = do
  str <- putStrLn "Please input the message do you want to decrypt for " >> getLine 
  (putStrLn . decryptS2S priKey) str --maybe (putStrLn "Wrong format.") (putStrLn . decryptS2S priKey) $ str

takeAction priKey (Decrypt (Just deObj)) = do
  case deObj of
    Message str -> do
      --putStrLn str
      (putStrLn . decryptS2S priKey) str --maybe (putStrLn "Wrong format.") (putStrLn . decryptS2S priKey) $ str
    File fp -> do 
      putStrLn $ "Decrypting file " ++ show fp ++ " ..."
      decryptFile priKey fp
      putStrLn "File decrypted."

takeAction p NothingSpecified = do
  putStrLn "There is no parameter input, or they are of bad format. What do you want to do?"
  putStrLn "1.Encrypt message"
  putStrLn "2.Encrypt file"
  putStrLn "3.Decrypt message"
  putStrLn "4.Decrypt file"
  putStrLn "5.Display help"
  choice <- safeInput "Please input(1-5):" [((>5), "too large"), ((<0), "cannot be negative")]
  case (choice :: Int) of
    1 -> do 
      target <- safeInputStr "Please specify who do you want to encrypt for: " [(null, "Cannot be empty")]
      takeAction p $ Encrypt (Just target) Nothing
    2 -> do
      target <- safeInputStr "Please specify who do you want to encrypt for: " [(null, "Cannot be empty")]
      filepath <- safeInputStr "Please specify the file path: " [(null, "Cannot be empty")]
      takeAction p $ Encrypt (Just target) (Just (File filepath))
    3 -> do
      str <- safeInputStr "Please input the encrypted message: " [(null, "Cannot be empty")]
      takeAction p (Decrypt (Just $ Message str))
    4 -> do
      str <- safeInputStr "Please input the filepath: " [(null, "Cannot be empty")]
      takeAction p (Decrypt (Just $ File str))
    5 -> takeAction p DisplayHelp
  getLine >> return ()

takeAction _ DisplayHelp = putStrLn $ foldl (\x y -> x ++ "\n" ++ y) ""
  [ "一个简单易用的 RSA公匙加密体系实现。"
  , ""
  , "初次使用：运行并按照提示生成私匙"
  , "可以创建一个快捷方式，方便拖动文件。"
  , ""
  , "将你的.contact文件分享给你的朋友。"
  , "不要分享.prikey"
  , ""
  , "任何人只需要有公匙（contact里面包含公匙）就可以给这个人发送加密消息"
  , "但是只有有私匙(private key)的人才能解密任何被这个公匙加密的消息"
  , ""
  , "导入别人的联系方式："
  , "将.contact文件拖动到prime.exe上。你也可以直接编辑allcontacts.txt。"
  , ""
  , "加密：在命令行下运行"
  , ".\\Prime -to <谁谁谁> -file <filepath>"
  , ".\\Prime -to <谁谁谁> \"message\""
  , ".\\Prime -to <谁谁谁> "
  , ".\\Prime -to <谁谁谁> -string \"message\""
  , "或：拖动任意文件到exe上面也可以加密文件"
  , " "
  , "解密：在命令行下运行"
  , ".\\Prime -d "
  , ".\\Prime -d \"message\""
  , ".\\Prime -d -file filepath"
  , "或：拖动 .encrypt 文件到exe上面也可以解密文件"
  , ""
  , ""
  , "注："
  , ""
  , "由于windows 命令行的阴间特性"
  , "如果你用的是拖动，被拖动的文件需要和prikey在同一个文件夹才能正常工作"
  , "不过，创建快捷方式并拖动到快捷方式上可以解决此问题（"
  , "不然你拖一个别的文件夹的文件上去，它会提示你创建prikey, 但实际上你已经在prime.exe同文件夹下创建过了"
  , "By Eiko"
  ]


encryptTo ctc (File fp) = do
  encryptFile (publicKey ctc) fp
  putStrLn $ "File encrypted, saved as " ++ show fp ++ ".encrypt . You can send it to " ++ name ctc

encryptTo ctc (Message msg) = do
  let encrypted = encryptS2S (publicKey ctc) msg
  print encrypted

main :: IO ()
--main = test


main = do
  args <- getArgs
  keyExists <- doesFileExist privatePath
  if keyExists then do
    pri_str <- readFile privatePath
    let priKey = read pri_str :: PrivateKey
        pubKey = privateToPublic priKey
    --seq priKey $ putStrLn "keys loaded."
    case MP.mRunParserF parseArgs args of
      Nothing -> return ()
      Just act -> takeAction priKey act -- >> (getLine >> return ())
--    str <- getLine
--    print $ encryptS pubKey str
--    putStrLn $ decryptS priKey $ encryptS pubKey str
  else do
    putStrLn $ "No " ++ show privatePath ++ " found, need to generate new keys"
    strength <- readStrength
    putStrLn "Generating Keys for you, please wait..."
    keys@(priKey, pubKey) <- genKeyPair strength
    print keys
    writeFile privatePath ((show :: PrivateKey -> String) priKey)
    writeFile publicPath ((show :: PublicKey -> String) pubKey)
    putStrLn $ "Key pair generated and saved to " 
             ++ show privatePath 
             ++ " and " 
             ++ show publicPath 
             ++ ". Do not share your private key with any one, keep it safe! \
             \You can share your public key to other people so that people can send encrypted messages to you."
    name_str <- safeInputStr "Input your name to create your contact card: " 
                          $ [((>100) . length, "too long"), ((<2) . length, "too short")] 
                          ++ [((char `elem`), "cannot contain '" ++ [char] ++ "'") | char <- ['\\', '/', ':', '.']]
    email_str <- maybeInputStr "Input your email (or leave it empty, press Enter): "
    let meContact = Contact {name = name_str, metaData = MetaData {email = email_str, memo = Nothing}, publicKey = pubKey}
    writeFile (name_str ++ myCardPath) (show meContact)
    detectAndCreateCTCs
    putStrLn $ (name_str ++ myCardPath) ++ " file saved, share it to other people to build their contact lists. This file only contains your name, your email, and your public key. You can drag .contact files to import other people's public keys to your contact list, the list is saved in " ++ show contactsPath ++ " ."
    case MP.mRunParserF parseArgs args of
      Nothing -> return ()
      Just act -> takeAction priKey act
    where
      readStrength = safeInput "Please input the strength (length) of your keys (integer 0~2000, recommended 200, the larger the safer, but takes much more time to compute): " [((> 2000), "too large"), ( (< 0), "cannot be nagative")]
