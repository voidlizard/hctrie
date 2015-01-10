-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Typeable

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
  cshow = dquotes . pretty . B8.unpack
  ctype _ = ["char *"]
  
instance CShow Int where
  ctype _ = ["int"]

-- | Type that represent any unknown at compile time C value,
-- this type is used for the scalar values
data SomeCValue = forall a . (Show a, CShow a, Typeable a, Ord a) => SomeCValue a

deriving instance Typeable SomeCValue

instance Show SomeCValue where
  show (SomeCValue x) = "SomeCValue " ++ show x

instance CShow SomeCValue where
  cshow (SomeCValue x) = cshow x
  ctype (SomeCValue x) = ctype x

instance Eq SomeCValue where
  (SomeCValue x) == (SomeCValue y) =
     maybe False (== x) (cast y)

instance Ord SomeCValue where
  (SomeCValue x) `compare` (SomeCValue y) =
      case cast y of
        Just y' -> x `compare` y'
	Nothing -> typeOf x `compare` typeOf y

-- | Type that represents any unknown at compile time C recoed
newtype SomeCRecord = SomeCRecord [SomeCValue] deriving (Typeable)

instance CShow SomeCRecord where
  cshow (SomeCRecord xs) = encloseSep "{" "}" ","
                         $ map (\(SomeCValue x) -> cshow x) xs
  ctype (SomeCRecord xs) = concatMap ctype xs

instance Show SomeCRecord where
  show (SomeCRecord xs) = "SomeCRecord "++ showList xs ""

instance Eq  SomeCRecord where (SomeCRecord xs) == (SomeCRecord ys) = xs == ys
instance Ord SomeCRecord where (SomeCRecord xs) `compare` (SomeCRecord ys) = xs `compare` ys
