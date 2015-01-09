-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
module Language.C.Generate.Parse
  ( parseFields
  , ParseValue(..)
  , Conf(..)
  , parseCSVLike
  ) where

import Language.C.Generate.Types

import           Control.Applicative
import           Control.Monad (void)
import           Data.Maybe (catMaybes)
import           Data.ByteString (ByteString)
import           Data.Word

import Data.ByteString.Internal (c2w)
import           Data.Attoparsec.ByteString.Char8 as A

-- | Values that are supported in parser
data ParseValue = PVInt Int
                | PVBS  ByteString
                deriving (Eq, Show)

class IsParseValue a where
  toParseValue :: a -> ParseValue

instance IsParseValue Int where
  toParseValue = PVInt

instance IsParseValue ByteString where
  toParseValue = PVBS

-- | Convert a list of parsed values into some 'SomeCValue'
parseFields :: [ParseValue] -> SomeCValue
parseFields []  = error "fields should contain at least one value"
parseFields [x] = parseField x
parseFields xs  = SomeCValue (SomeCRecord (map parseField xs))

parseField :: ParseValue -> SomeCValue
parseField (PVInt i)  = SomeCValue i
parseField (PVBS  s)  = SomeCValue s

data Conf = Conf [Word8] SomeCValue deriving (Eq, Show)

conf :: Parser [Conf]
conf = fmap (\(k, v) -> Conf k (parseFields v)) . catMaybes <$> confLine `sepBy` endOfLine

delimeter :: Char
delimeter = ','

confLine :: Parser (Maybe ([Word8], [ParseValue]))
confLine = choice [ pure Nothing <* comment
                  , Just <$> line
		  ]
  where
    comment :: Parser ()
    comment = void "#"

    line :: Parser ([Word8], [ParseValue])
    line = (,) <$> key
               <*> many1 field

    key :: Parser [Word8]
    key = do
      c <- peekChar'
      bs <- case c of
        '"' -> quotedField   <?> "quoted key"
        _   -> unquotedField <?> "unquoted key"
      either fail return $ parseOnly unescape bs

    field :: Parser ParseValue
    field =  skipSpace
          *> char delimeter
	  *> skipSpace
          *> choice [ PVInt <$> decimal
                    , PVBS  <$> quotedField
                    , PVBS  <$> unquotedField
                    ]

unescape :: Parser [Word8]
unescape = many1 p
   where
      p = do
        c <- anyChar
        case c of
          '\\' -> do c' <- anyChar
	             case c' of
		       'x' -> liftA2 mkHex (c2w <$> satisfy (isHexDigit.c2w))
                                           (c2w <$> satisfy (isHexDigit.c2w))
                       _ -> choice [ liftA2 (mkDec c') digit digit
		                   , liftA  (mkDec '0' c') digit
				   , pure (c2w c' - 48)
				   ]
	  _    -> return $ c2w c

      isHexDigit w = (w >= 48 && w <= 57) ||
                     (w >= 97 && w <= 102) ||
                     (w >= 65 && w <= 70)
      mkHex :: Word8 -> Word8 -> Word8
      mkHex a b = (f a * 16) + f b
      f w | w >= 48 && w <= 57 = w - 48
          | w >= 97            = w - 87
          | otherwise          = w - 55
      mkDec a b c = ((c2w a - 48) * 100) + ((c2w b - 48) * 10) + c2w c - 48

quotedField :: Parser ByteString
quotedField = char '"' *> A.takeWhile1 (/= '"')  <* char '"'

unquotedField :: Parser ByteString
unquotedField = A.takeWhile1 (liftA2 (&&) (/= '\n') (/= delimeter))

parseCSVLike :: ByteString -> Either String [Conf]
parseCSVLike = parseOnly conf
