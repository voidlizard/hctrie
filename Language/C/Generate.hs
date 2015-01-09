-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
module Language.C.Generate
  ( -- * structure helpers
    function
  , if_
  , for_
  , block
    -- * types
  , uint8_t
  , uint16_t
  , uint32_t
  , uint64_t
    -- * missing functions
  , encloseSep'
  , findMaxType
  ) where

import Text.PrettyPrint.Leijen.Text

import Data.Word

uint8_t :: Doc
uint8_t = "uint8_t"

uint16_t :: Doc
uint16_t = "uint16_t"

uint32_t :: Doc
uint32_t = "uint32_t"

uint64_t :: Doc
uint64_t = "uint64_t"

function :: Doc -> Doc -> [Doc] -> Doc -> Doc
function tp name params body = tp <+> name <> tupled params <>
   nest 4 (lbrace <$> body) <$> rbrace

if_ :: Doc -> Doc -> Doc
if_ cls body = "if" <+> parens cls <>
  nest 4 (lbrace <$> body) <$> rbrace

for_ :: Doc -> Doc -> Doc -> Doc -> Doc
for_ from to by body = "for" <+> parens ( from <> semi <+> to <> semi <+> by) <>
  nest 4 (lbrace <$> body) <$> rbrace

block :: Doc -> Doc
block body = nest 4 (lbrace <$> body) <$> rbrace

-- Utilities
encloseSep' :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep' left right sep' ds
  = case ds of
        []  -> left <> right
	[d] -> left <> d <> right
	_   -> align (fillCat (zipWith (<>) (left : repeat sep') ds) <> right)

findMaxType :: Int -> Doc
findMaxType x
  | x <= fromIntegral (maxBound ::Word8)  = uint8_t
  | x <= fromIntegral (maxBound ::Word16) = uint16_t
  | x <= fromIntegral (maxBound ::Word32) = uint32_t
  | otherwise                             = uint64_t
