module Contacts where
import Prime
import Encryption
import Data.Set

data Contact = Contact
  { name :: String
  , metaData :: MetaData
  , publicKey :: PublicKey
  } deriving (Show, Eq, Ord, Read)

data MetaData = MetaData 
  { email :: Maybe String
  , memo  :: Maybe String
  } deriving (Show, Eq, Ord, Read)

type AllContacts = Set Contact

readAllContacts :: FilePath -> IO AllContacts
readAllContacts fp = do
  file <- readFile fp
  return $ read file

writeAllContacts :: FilePath -> AllContacts -> IO ()
writeAllContacts fp ac = do
  writeFile fp (show ac)
