-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
module LCB.Parse
  ( parseFile
  , Section(..)
  , Ini
  ) where

import           Control.Applicative
import           Control.Monad (void)
import           Data.Either (rights)
import           Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

type Title = ByteString
type Name  = ByteString
type Values = [ByteString]
type Ini = [Section]

data Section  = Section Title [(Name,Values)] deriving (Show)

ini :: Parser Ini
ini = rights <$> many ((Left <$> comment) <|> (Right <$> section))

comment :: Parser () 
comment = void $ string "#" *> A.takeWhile (/= '\n') <* endOfLine

section :: Parser Section
section = Section <$> title <*> values

title = skipSpace *> char '[' *> A.takeWhile ( /= ']') <* char ']' <* endOfLine

values :: Parser [(Name,Values)]
values = many value

value :: Parser (Name,Values)
value = do mx <- peekChar
           case mx of
	     Nothing -> fail "no more"
	     Just x
	       | isAlpha_ascii x -> (,) <$> A.takeWhile (/= '=') <* char '=' <*> vl
	       | x == '#'  -> comment >> value
	       | otherwise -> fail "no more"

vl =  (string "<<" *> multiline)
   <|> (((:[]) <$> A.takeWhile (/= '\n')) <* endOfLine)

multiline = do
   eot <- A.takeWhile (/= '\n') <* endOfLine
   let loop = do
         x   <- A.takeWhile (/= '\n') <* endOfLine
         if x == eot
	 then return []
	 else (x:) <$> loop
   loop 

parseFile :: String -> IO (Either String Ini)
parseFile fn = parseOnly ini <$> B.readFile fn
