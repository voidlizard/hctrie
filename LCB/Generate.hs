-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
-- TODO: 
--   * Read prefixes
--   * Add documentation
--   * Add documentation into output code
--   * Generate header files
--   * Support total alphabet
{-# LANGUAGE ViewPatterns #-}
module LCB.Generate
  ( generate 
  , generateTests
  , generateFiles
  , lookupG
  ) where

import Language.C.Generate.Types

import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy    as Text
import Data.Char

import qualified Data.ByteString.Char8 as B8
import           Data.List
import           Data.Maybe
import qualified Data.Map as Map
import           Data.TrieMap (T(..))

uint8_t :: Doc
uint8_t = "uint8_t"

prefixed :: String -> String -> String
prefixed "" x = x
prefixed c  x = c ++ '_':x

generateFiles :: String
              -> T Int Int
              -> [(t, (Int, Map.Map Int Int))]
              -> [Int]
              -> [B8.ByteString]
              -> [[Int]]
              -> [(String, Doc)]
generateFiles p t v a r ts = 
     [ (prefixed p "radix.c",       generate p v a r)
     , (prefixed p "radix.h",       generateHeader p)
     , (prefixed p "radix_tests.c", generateTests p t a r ts)
     ]

generate :: CShow a
         => String
         -> [(t, (Int, Map.Map Int Int))]
         -> [Int]
         -> [a]
         -> Doc
generate p v a r = vcat 
     [ "#include" <+> "<stdint.h>"
     , "#include" <+> dquotes "radix.h"
     , "#define" <+> "CHUNK_NUM"   <+> int chunksNo
     , "#define" <+> "ALPHABET"    <+> int alphabetSize 
     , "#define" <+> "RESULTS_NUM" <+> int resultsSize
     , linebreak
     , "static" <+> uint8_t <+> "encode_tbl" <> "[]" <+> "=" <+> 
         encloseSep' lbrace rbrace "," (map int (buildAlphabet a)) <> semi
     , linebreak
     , "static" <+> "int" <+> "chunks" <> brackets "CHUNK_NUM" <> brackets "ALPHABET+2" <+> "=" <+>
         enclose lbrace rbrace
	    (align (fillCat $ (map (mkChunk  alphabetSize) v))) -- TODO: use nest
	 <> semi
     , linebreak
     , "char*" <+> "results" <> brackets "RESULTS_NUM" <+> "=" <+>
         encloseSep' lbrace rbrace ", " (map cshow r) <> semi
     , linebreak
     , "int" <+> (string $ Text.pack $ prefixed p "radix_trie")
             <> (tupled [ "void"   <+> "*cc"
                        , (string $ Text.pack $ prefixed p "radix_trie_clb_t") <+> "*cb"
                        ]
              ) <> (nest 4 (lbrace <$> inner) <$> rbrace)
     ]
     where
       inner = vcat 
	 [ "int i = 0" <> semi
	 , "int consumed = 0" <> semi
	 , nest 4 ("do" <+> lbrace <$> do1) <$> rbrace <+> "while (1)" <> semi
	 , "if (!chunks[i][0]) { return 0; }" <> "// no value is associated with node"
	 , "cb->consume_result(cc, results[chunks[i][0]], consumed, chunks[i][1])" <> semi
	 ]
       do1   = vcat
	 [ nest 4 (text "if (!cb->has_more_input(cc))" </> "break" <> semi)
         , uint8_t <+> "c" <+> "=" <+> "encode_tbl[cb->get_input(cc)]" <> semi
	 , nest 4 ("if (c == 0)" </> "break" <> semi)
	 , "int next = chunks[i][c+1]" <> semi <+> "// zero is ommited, (not a case in full alphabet)"
	 , "if (next == 0) break" <> semi
	 , "i = next" <> semi
	 , "++consumed" <> semi
	 ]
       chunksNo     = length v
       resultsSize  = length r
       alphabetSize = length a


generateHeader :: String -> Doc
generateHeader p = vcat
    [ "#ifndef" <+> (string $ Text.pack $ prefixed (map toUpper p) "RADIX_TREE_H")
    , "#define" <+> (string $ Text.pack $ prefixed (map toUpper p) "RADIX_TREE_H")
    , "#include" <+> "<stdint.h>"
    , linebreak
    , "typedef" <+> "struct" <+> radix_trie_clb <>
        nest 4 (lbrace <$> vcat
                       [ "int"    <+> parens ("*" <> "has_more_input") <+> parens ("void *") <> semi
                       , uint8_t  <+> parens ("*" <> "get_input") <+> parens ("void *") <> semi
                       , "int"    <+> parens ("*" <> "consume_result")
                                  <+> tupled [ "void *"
                                             , "char *"
                                             , "int"
                                             , "int" ] <> semi
                      ]) <$> rbrace <+> radix_trie_clb <> "_t" <> semi
    , "int" <+> (string $ Text.pack $ prefixed p "radix_trie")
            <> (tupled [ "void"   <+> "*cc"
                       , radix_trie_clb <> "_t" <+> "*callback"
                       ]) <> semi
    , "#endif"
    ]
    where
      radix_trie_clb = (string $ Text.pack $ prefixed p "radix_trie_clb") 

function :: Doc -> Doc -> [Doc] -> Doc -> Doc
function tp name params body = tp <+> name <> tupled params <>
   nest 4 (lbrace <$> body) <$> rbrace

if_ :: Doc -> Doc -> Doc
if_ cls body = "if" <+> parens cls <>
  nest 4 (lbrace <$> body) <$> rbrace

generateTests :: String
              -> T Int Int
              -> [Int]
              -> [B8.ByteString]
              -> [[Int]]
              -> Doc
generateTests p t a v inputs = vcat
    [ "#include <stdint.h>" 
    , "#include <stdlib.h>"
    , "#include <stdio.h>"
    , "#include" <+> dquotes (string $ Text.pack $ prefixed p "radix.h")
    , linebreak
    , "#define" <+> "TESTS_SIZE" <+> int (length inputs)
    , linebreak
    , "int" <+> "inputs[TESTS_SIZE][500]" <+> "=" <+> 
        enclose lbrace rbrace
	  (align (fillCat $ (map (\is -> encloseSep' lbrace rbrace "," (int (length is):map int is) <$$> ",") inputs)))
	  <> semi
    , "int" <+> "result[TESTS_SIZE]" <+> "=" <+> 
	enclose lbrace rbrace
	  (align (fillCat (intersperse "," $ map int results))) <> semi
    , "int" <+> "should_match[TESTS_SIZE]" <+> "=" <+>
        enclose lbrace rbrace
	  (align (fillCat (intersperse "," $ map int matched))) <> semi
    , "int" <+> "should_consume[TESTS_SIZE]" <+> "=" <+>
        enclose lbrace rbrace
	  (align (fillCat (intersperse "," $ map int consumed))) <> semi
    , "char *" <+> "should_value[TESTS_SIZE]" <+> "=" <+>
        encloseSep' lbrace rbrace ", " (map (\i -> dquotes.(<>"\\0").pretty.B8.unpack $ (v!!i)) values) <> semi
    , linebreak
    , "static" <+> "char *" <+> "current_value"   <+> "=" <+> "NULL" <> semi
    , "static" <+> "int"    <+> "current_matched"  <+> "=" <+> int 0  <> semi
    , "static" <+> "int"    <+> "current_consumed" <+> "=" <+> int 0  <> semi
    , "static" <+> "int"    <+> "input_idx"        <+> "=" <+> int 0  <> semi
    , "static" <+> "int"    <+> "input_size"       <+> "=" <+> int 0  <> semi
    , "static" <+> "int *"  <+> "input"            <+> "=" <+> "NULL" <> semi
    , function uint8_t "feed_input" ["void * cc"] $ vcat
        [ "if" <+> parens ("input_idx > input_size") <+> "return 0;"
        , "return input[input_idx++];"
        ]
    , function "int" "has_more" ["void * cc"] $ vcat
        [ "return (input_idx <= input_size);" ]
    , function "int"   "dump_output" [ "void * cc", "char * result", "int consumed", "int exact"] $ vcat
        [ "current_value = result" <> semi
	, "current_matched = exact" <> semi
	, "current_consumed = consumed" <> semi
	, "return 1" <> semi
	]
    , function "int" "main" ["int argc", "char *argv[]"] $ vcat
        [ "int i =0"     <> semi
        , (string $ Text.pack $ prefixed p "radix_trie_clb_t") <+> "cb" <+> "="
	        <+> encloseSep lbrace rbrace "," ["has_more", "feed_input", "dump_output" ] <> semi
        , "for" <+> parens ("i" <> "=" <> "0" <> ";" <+> "i" <> "<" <> "TESTS_SIZE;" <+> "i++") <>
            nest 4 (lbrace <$> vcat
              [ "input_idx  = 1" <> semi
	      , "input_size = inputs[i][0]" <> semi
	      , "input      = inputs[i]"    <> semi
              , "int current_result  =" <+> (string $ Text.pack $ prefixed p "radix_trie")
                                        <> "(0, &cb)" <> semi
	      , if_ "current_result != result[i]" $ vcat
	            [ "printf(\"%i: [Error: wrong result %i, should be %i]\\n\",i,current_result,result[i])" <> semi
		    , "continue" <> semi
		    ]
              , if_ "current_result" $ vcat 
	            [ if_ "current_matched != should_match[i]" $ vcat
                         [ "printf(\"%i: [Error: wrong matched %i, should be %i]\\n\",i,current_matched,should_match[i])" <> semi
                         , "continue" <> semi
                         ]
                    , if_ "current_consumed != should_consume[i]" $ vcat
                        [ "printf(\"%i: [Error: wrong consumed %i, should be %i]\\n\",i,current_consumed,should_consume[i])" <> semi
                        , "continue" <> semi
                        ]
                    , if_ "strcmp(should_value[i], current_value)" $ vcat
                        [ "printf(\"%i: [Error: values not match %s, against %s]\\n\",i,current_value,should_value[i])" <> semi
                        , "continue" <> semi
                        ]
                    ]
              , "printf(\"%i: OK\\n\", i)" <> semi
              ]) <$> rbrace
        ]
    ]
  where
    (results, matched, consumed, values) = unzip4 $ map (lookupG t . recode) inputs
    recode  = map (\i -> fromJust $ i `elemIndex` ((-1):a))


lookupG :: (T Int Int) -> [Int] -> (Int, Int, Int, Int)
lookupG = go 0 
  where go c (T v m) [] 
            = (fromEnum (v/=0), fromEnum (Map.null m), c, v)
	go c (T _ m) ((\x -> x `Map.lookup` m -> Just t) :xs)
	    = go (c+1) t xs
	go c (T v _) _
	    = (fromEnum (v/=0), 0, c, v)


buildAlphabet :: [Int] -> [Int]
buildAlphabet = go 0 1
  where
    go c v (x:xs)
      | c == 256  = []
      | c == x    = v:go (c+1) (v+1) xs
      | otherwise = 0:go (c+1) v     (x:xs)
    go c _ []     = replicate (256-c) 0

mkChunk :: (Ord k, Num k, Enum k)
        => k
	-> (t, (Int, Map.Map k Int))
	-> Doc
mkChunk k (_,(i, m)) = encloseSep' lbrace rbrace "," (int i:typ:map int lst) <$$> ","
  where
    lst = [fromMaybe 0 (Map.lookup j m) | j <- [1..k]]
    typ 
      | Map.null m = int 1 
      | otherwise  = int 0 

-- Utilities
encloseSep' :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep' left right sep' ds
  = case ds of
        []  -> left <> right
	[d] -> left <> d <> right
	_   -> align (fillCat (zipWith (<>) (left : repeat sep') ds) <> right)
