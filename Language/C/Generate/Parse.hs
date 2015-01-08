module Language.C.Generate.Parse 
  ( parseFields
  , ParseValue(..)
  ) where

import Language.C.Generate.Types

import Data.ByteString (ByteString)

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
