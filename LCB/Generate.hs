-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
-- TODO: 
--   * Read prefixes
--   * Add documentation
--   * Add documentation into output code
--   * Generate header files
--   * Support total alphabet
module LCB.Generate
  ( generate 
  , output
  ) where

import Text.PrettyPrint.Leijen.Text
import Data.Text.Lazy.IO as Text
import Data.Char

import qualified Data.ByteString.Char8 as B8
import           Data.Maybe
import qualified Data.Map as Map

uint8_t :: Doc
uint8_t = "uint8_t"

generate v a r = vcat 
     [ "#include" <+> "<stdint.h>"
     , "#define" <+> "CHUNK_NUM"   <+> int chunksNo
     , "#define" <+> "ALPHABET"    <+> int alphabetSize 
     , "#define" <+> "RESULTS_NUM" <+> int resultsSize
     , linebreak
     , uint8_t <+> "encode_tbl" <> "[]" <+> "=" <+> 
         encloseSep' lbrace rbrace "," (map int (buildAlphabet a)) <> semi
     , linebreak
     , "int" <+> "chunks" <> brackets "CHUNK_NUM" <> brackets "ALPHABET+2" <+> "=" <+>
         enclose lbrace rbrace
	    (align (fillCat $ (map (mkChunk  alphabetSize) v))) -- TODO: use nest
	 <> semi
     , linebreak
     , "char*" <+> "results" <> brackets "RESULTS_NUM" <+> "=" <+>
         encloseSep' lbrace rbrace ", " (map (dquotes.(<>"\\0").pretty.B8.unpack) r) <> semi
     , linebreak
     , "int" <+> "function" <> (tupled [ "void"   <+> "*cc"
                                       , "int"    <+> parens ("*" <> "has_more_input") <+> parens ("void *")
				       , uint8_t  <+> parens ("*" <> "get_input") <+> parens ("void *")
				       , "int"    <+> parens ("*" <> "consume_result")
				                  <+> tupled [ "void *"
						             , "char *"
							     , "int"
							     , "int"] -- TODO
				       ]
                                       ) <>
         (nest 4 (lbrace <$> inner) <$> rbrace)
     ]
     where
       inner = vcat 
	 [ "int i = 0" <> semi
	 , "int consumed = 0" <> semi
	 , nest 4 ("do" <+> lbrace <$> do1) <$> rbrace <+> "while (1)" <> semi
	 , "if (chunks[i][0]) { return 0; }" <> "// no value is associated with node"
	 , "consume_result(cc, results[chunks[i][0]], consumed, chunks[i][1])" <> semi
	 ]
       do1   = vcat
	 [ nest 4 (text "if (!has_more_input(cc))" </> "break" <> semi)
         , uint8_t <+> "c" <+> "=" <+> "decode(get_input(cc))" <> semi
	 , nest 4 ("if (c == 0)" </> "break" <> semi)
	 , "int next = chunks[i][c+2]" <> semi
	 , "if (next == 0) break" <> semi
	 , "i = next" <> semi
	 , "++consumed" <> semi
	 ]
       chunksNo     = length v
       resultsSize  = length r
       alphabetSize = length a


buildAlphabet :: [Int] -> [Int]
buildAlphabet = go 0 1
  where
    go c v (x:xs)
      | c == 256  = []
      | c == x    = v:go (c+1) (v+1) xs
      | otherwise = 0:go (c+1) v     (x:xs)
    go c _ []     = replicate (256-c) 0

chunkNothing = int 0
chunkValue   = int 1

mkChunk k (_,(i, m)) = encloseSep' lbrace rbrace "," (int i:typ:map int lst) <$$> ","
  where
    lst = [fromMaybe 0 (Map.lookup j m) | j <- [1..k]]
    typ 
      | Map.null m = chunkValue
      | otherwise  = chunkNothing
    

output g = Text.putStrLn $ displayT $ renderPretty 0.8 80 g


-- Utilities
encloseSep' :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep' left right sep ds
  = case ds of
        []  -> left <> right
	[d] -> left <> d <> right
	_   -> align (fillCat (zipWith (<>) (left : repeat sep) ds) <> right)
