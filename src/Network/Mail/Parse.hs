module Network.Mail.Parse (parseMessage) where

import           Network.Mail.Parse.Parsers.Message (messageParser)
import           Network.Mail.Parse.Types

import           Control.Monad                      (join)
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8              as BSC
import           Data.Either.Combinators            (mapLeft)
import qualified Data.Text                          as T

-- |Parses a single message of any mimetype
parseMessage :: BSC.ByteString -> Either ErrorMessage EmailMessage
parseMessage message =
  join . mapLeft T.pack $ parseOnly (messageParser Nothing Nothing) message
