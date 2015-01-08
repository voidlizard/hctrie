-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
module Language.C.Generate
  ( -- * structure helpers
    function
  , if_
    -- * types
  ,  uint8_t
    -- * missing functions
  , encloseSep'
  ) where

import Text.PrettyPrint.Leijen.Text

uint8_t :: Doc
uint8_t = "uint8_t"

function :: Doc -> Doc -> [Doc] -> Doc -> Doc
function tp name params body = tp <+> name <> tupled params <>
   nest 4 (lbrace <$> body) <$> rbrace

if_ :: Doc -> Doc -> Doc
if_ cls body = "if" <+> parens cls <>
  nest 4 (lbrace <$> body) <$> rbrace

-- Utilities
encloseSep' :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep' left right sep' ds
  = case ds of
        []  -> left <> right
	[d] -> left <> d <> right
	_   -> align (fillCat (zipWith (<>) (left : repeat sep') ds) <> right)
