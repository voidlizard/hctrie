-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.C.Generate.Types
  ( CShow(..)
  , SomeCValue(..)
  , SomeCRecord(..)
  ) where

import Text.PrettyPrint.Leijen.Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

-- | Encapsulate functions that are required in order to show
-- a value in generated C code.
class CShow a where
  -- | Shows value in C code.
  cshow :: a -> Doc
  default cshow :: Pretty a => a -> Doc
  cshow = pretty
  -- | Shows type of the corresponding C value
  ctype :: a -> [Doc]

instance CShow ByteString where
  cshow = dquotes . (<> "\\0") . pretty . B8.unpack
  ctype _ = ["char *"]
  
instance CShow Int where
  ctype _ = ["int"]

-- | Type that represent any unknown at compile time C value,
-- this type is used for the scalar values
data SomeCValue = forall a . (Show a, CShow a) => SomeCValue a

instance Show SomeCValue where
  show (SomeCValue x) = "SomeCValue " ++ show x

instance CShow SomeCValue where
  cshow (SomeCValue x) = cshow x
  ctype (SomeCValue x) = ctype x

-- | Type that represents any unknown at compile time C recoed
newtype SomeCRecord = SomeCRecord [SomeCValue]

instance CShow SomeCRecord where
  cshow (SomeCRecord xs) = encloseSep "{" "}" ","
                         $ map (\(SomeCValue x) -> cshow x) xs
  ctype (SomeCRecord xs) = concatMap ctype xs

instance Show SomeCRecord where
  show (SomeCRecord xs) = "SomeCRecord "++ showList xs ""
