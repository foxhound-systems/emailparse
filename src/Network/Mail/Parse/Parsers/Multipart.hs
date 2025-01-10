module Network.Mail.Parse.Parsers.Multipart (parseMultipart) where

import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import           Data.Word8

import           Network.Mail.Parse.Utils

isBoundaryMatched :: BSC.ByteString -> Int -> Word8 -> Maybe Int
isBoundaryMatched boundary matchIdx char =
  if char == BS.index boundary matchIdx
    then if matchIdx == boundaryLength - 1
          then Nothing
          else Just $ matchIdx + 1
    else Just 0
  where boundaryLength = BS.length boundary

trimPayload :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
trimPayload boundary payload = BSC.take trimLength payload
  where payloadLength  = BSC.length payload
        boundaryLength = BSC.length boundary
        trimLength     = payloadLength - boundaryLength + 1

parseMultipart :: BSC.ByteString -> Parser [BSC.ByteString]
parseMultipart boundary =
  do
    _ <- manyTill' anyWord8 (string completeBoundary) <* consumeTillEndLine
    payloads <- many' (scan 0 (isBoundaryMatched completeBoundary) <* consumeTillEndLine)
    return $ map (trimPayload completeBoundary) payloads
  where completeBoundary = BSC.append "--" boundary
