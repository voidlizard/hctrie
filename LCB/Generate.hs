-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--  XXX: Support total alphabet
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module LCB.Generate
  ( generateFiles
--  , generateTests
--  , generateFiles
--  , lookupG
  , convertTrie
  , numerate
  , normalizeValues
  , prepareNode
  ) where

import           Prelude hiding (all)
import Language.C.Generate.Types
import Language.C.Generate
import           Data.TrieMap (T(..))
import           Data.TrieMap.Utils hiding (recode)

import Text.PrettyPrint.Leijen.Text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy    as Text

import qualified Control.Applicative as A
import           Data.Foldable hiding (for_)
import           Data.Traversable
import           Data.List hiding (mapAccumL, all)
import           Data.Maybe
import qualified Data.Monoid as M
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

prefixed :: Text -> Text -> Text 
prefixed "" x = x
prefixed c  x = c M.<> "_" M.<> x

alphabetMaxSize :: Int
alphabetMaxSize = 256

data Node a b
   = Node { nodeValue      :: b
--          , nodeIsTerminal :: Bool
          , nodeData       :: NodeData a b
          }
   deriving (Show)

instance Functor (Node a) where
  fmap f (Node v d) = Node (f v) (fmap f d)


instance Foldable (Node a) where
  foldMap f (Node v d) = f v M.<> foldMap f d
 
instance Traversable (Node a) where
  traverse f (Node v d) = Node A.<$> f v
                               A.<*> traverse f d
  
data NodeData a b
   = NodeSwitch { _nodeLinks  :: Map a (Node a b) }
   | NodeChunk  { _nodeValues :: [a]
                , _nodeNext   :: Maybe (Node a b)
                }
   deriving (Show)

instance Functor (NodeData a) where
  fmap f (NodeSwitch m)  = NodeSwitch (Map.map (fmap f) m)
  fmap f (NodeChunk a m) = NodeChunk a (fmap (fmap f) m)

instance Foldable (NodeData a) where
  foldMap f (NodeSwitch m)         = M.mconcat $ map (foldMap f) $ Map.elems m
  foldMap _ (NodeChunk _ Nothing)  = M.mempty
  foldMap f (NodeChunk _ (Just x)) = foldMap f x

instance Traversable (NodeData a) where
  traverse f (NodeSwitch m)         = NodeSwitch A.<$> traverse (traverse f) m
  traverse _ (NodeChunk a Nothing)  = A.pure (NodeChunk a Nothing)
  traverse f (NodeChunk a (Just x)) = NodeChunk a . Just A.<$> traverse f x


convertTrie :: T a (Packed a (Either b b)) -> Node a (Bool, Maybe b)
convertTrie (T Nothing m) = Node (False, Nothing) (NodeSwitch (Map.map convertTrie m))
convertTrie (T (Just v) m) = case v of
    Packed ls _ -> n $ NodeChunk ls next
    NonPacked _ -> n $ NodeSwitch (Map.map convertTrie m)
  where
    n d = Node { nodeValue = maybe (False, Nothing) (\(f,k) -> (f, Just k))
                           $ fmap (either (False,) (True,)) (packedValue v)
               , nodeData = d
               }
    next
      | Map.null m = Nothing
      | otherwise  = Just $ n $ NodeSwitch (Map.map convertTrie m)

numerate :: Node a b -> Node a (Int, b)
numerate = snd . mapAccumL (\c b -> (succ c,(c,b))) 0 

normalizeValues :: Ord b => Node a (Bool, Maybe b) -> ([b], Node a (Bool, Maybe Int))
normalizeValues n = (l, n')
  where (s, n') = mapAccumL (\z b -> ( maybe s (flip Set.insert z) (snd b)
                                     , fmap (\x -> fmap succ . (`elemIndex` l) =<< x) b)) Set.empty n
        l = Set.toList s

prepareNode :: CShow b => Int -> Node Int (Int, (Bool, Maybe b)) -> Node Int Doc
prepareNode sz (Node (_,(t,mi)) d) = Node (encloseSep' lbrace rbrace "," (maybe "0" cshow mi:int (toNodeType t d):go d) <$$> ",")
                                         (node d)
  where toNodeType :: Bool -> NodeData a b -> Int
        toNodeType False NodeSwitch{} = 0
        toNodeType True  NodeSwitch{} = 1
        toNodeType False NodeChunk{}  = 2
        toNodeType True  NodeChunk{}  = 3
        go (NodeSwitch m) = [maybe (int 0) (\(Node (w,_) _) -> int w) $ i `Map.lookup` m | i  <- [1..sz]]
        go (NodeChunk l n) = int (length l):map int l ++ [maybe "0" (\(Node (w,_) _) -> int w) n]
        node (NodeSwitch m)  = NodeSwitch $ Map.map (prepareNode sz) m
        node (NodeChunk l m) = NodeChunk l $ fmap (prepareNode sz) m

generateFiles :: (CShow a, Ord a, Show a)
              => Text 
              -> Text 
              -> Bool  -- ^ Generate tests
              -> [Text]
              -> [Int] -- alphabet
              -> T Int (Packed Int (Either a a))
              -> [([Int],(Maybe a,Bool,Int))]
              -> [Either Text (Text, Doc)]
generateFiles p structName genTests hdr alphabet t tests = (map Right 
    [ (prefixed p "radix.c", generateFile)
    , (headerFileName,       generateHeader)
    ]) ++ (if genTests
          then if canGenTests
               then [Right (prefixed p "radix_tests.c", generateTests)]
               else [Left  "Warning: tests files were not generated, tests interface for 'char *' is not supported"]
          else [])
 where
   headerFileName = prefixed p "radix.h"
   radixTrie    = string $ prefixed p "radix_trie_lookup"
   radixTrieClb = string $ prefixed p "radix_trie_clb"

   -- Tree
   nd = convertTrie t
   (values, ndn) = normalizeValues nd
   ndd           = prepareNode alphabetSize
		 $ numerate ndn
   chunksList    = toList ndd

   -- Values
   chunkType = findMaxType (length chunksList)
   alphabetSize = length alphabet
   fullAlphabet = alphabetSize == alphabetMaxSize
   canGenTests :: Bool
   canGenTests = all (/="char *") $ ctype (head values)
   ctp :: Doc
   ctp = case ctype (head values) of
           [y] -> string $ Text.pack y
           _  | structName == "" -> error "struct option should be provided"
              | otherwise  -> string structName
   -- Pretty
   includes = vcat $
     (map (("#include" <+>) . string) hdr)
   generateFile = vcat
     [ includes
     , "#include" <+> dquotes (string headerFileName)
     , define_ "CHUNK_NUM"   $ length chunksList
     , define_ "ALPHABET"      alphabetSize
     , define_ "RESULTS_NUM" $ length values
     , encodeTbl
     , chunks
     , results
     , function "void *" radixTrie
                      [ "void"   <+> "*cc"
                      , radixTrieClb <> "_t" <+> "*cb"
                      ] $ vcat
          [ chunkType <+> "i = 0;"
          , "int consumed = 0;"
          , doWhile_ "1" $ vcat 
              [ chunkType <+> "next = 0;"
              , uint8_t <+> "s =  0;"
              , "switch(chunks[i][1])" <>
                  block (vcat
                    [ "case 0:"
                    , "case 1:"
                    , indent 4 $ vcat
                        [ next
                        , nextChunk
                        , "if (next == 0) goto result;"
                        , "++consumed;"
                        , "break;"]
                    , "case 2:"
                    , "case 3:"
                    , indent 4 $ vcat
                       [ for_ "" "s < chunks[i][2]" "s++" $ vcat 
                           [ next
                           , if_ "c != chunks[i][3+s]" "goto result;"
                           , "++consumed;"
                           ]
                       , if_ "(s==chunks[i][2])"
                             "next = chunks[i][3+s];"
                       ]
                    ])
              , "if (next == 0) break;"
              , "i = next;"
              ]
          , "result:"
          , "if (!chunks[i][0]) { return 0; }" <> "// no value is associated with node"
          , "return (cb->match(cc, &results[chunks[i][0]-1], consumed, chunks[i][1] & 1));"
          ]
     ]
     where
       -- alphabet related functions
       encodeTbl
         | fullAlphabet = empty
         | otherwise = "static" <+> uint8_t <+> "encode_tbl" <> "[]" <+> "=" <+> 
             encloseSep' lbrace rbrace "," (map int [ maybe 0 succ (a `elemIndex` alphabet) | a <- [0..255]]) <> semi
       encode :: Doc
       encode
         | fullAlphabet = uint8_t <+> "c" <+> "=" <+> "cb->get_input(cc)" <> semi
         | otherwise    = vcat 
            [ uint8_t <+> "c" <+> "=" <+> "encode_tbl[cb->get_input(cc)]" <> semi
            , nest 4 ("if (c == 0)" </> "goto result" <> semi)
            ]
       next :: Doc
       next = vcat
         [ "if" <+> parens ("!" <> "cb->has_more_input" <> parens ("cc")) </> "goto result;"
         , encode
         ]
       nextChunk :: Doc
       nextChunk
         | fullAlphabet = "next = chunks[i][c+2];"
         | otherwise    = "next = chunks[i][c+1];"
       chunks = "static" <+> chunkType <+> "chunks" <> brackets "CHUNK_NUM" <> brackets "ALPHABET+2" <+> "=" <+>
             enclose lbrace rbrace
               (align (fillCat $ toList ndd)) -- TODO: use nest
               <> semi
       results = ctp <+> "results" <> brackets "RESULTS_NUM" <+> "=" <+>
             encloseSep' lbrace rbrace ", " (map cshow values) <> semi

    -- Header file
   generateHeader = vcat
     [ "#ifndef" <+> (string $ Text.toUpper $ prefixed p "RADIX_TREE_H")
     , "#define" <+> (string $ Text.toUpper $ prefixed p "RADIX_TREE_H")
     , includes
     , "typedef" <+> "struct" <+> radixTrieClb <>
         nest 4 (lbrace <$> vcat
                        [ "int"    <+> parens ("*" <> "has_more_input") <+> parens ("void *") <> ";" 
                        , uint8_t  <+> parens ("*" <> "get_input") <+> parens ("void *") <> ";" 
                        , "void *" <+> parens ("*" <> "match")
                                   <+> tupled [ "void *"
                                              , ctp <+> "*"
                                              , "int"
                                              , "int" ] <> ";" 
                       ]) <$> rbrace <+> radixTrieClb <> "_t" <> ";" 
     , "void *" <+> radixTrie
                <> (tupled [ "void"   <+> "*cc"
                           , radixTrieClb <> "_t" <+> "*callback"
                           ]) <> semi
     , "#endif"
     ]

   generateTests = vcat
       [ "#include <stdlib.h>"
       , "#include <stdio.h>"
       , includes 
       , "#include" <+> dquotes (string headerFileName)
       , "#define" <+> "TESTS_SIZE" <+> int (length inputs)
       , "int" <+> "inputs[TESTS_SIZE][500]" <+> "="
               <+>  enclose lbrace rbrace
                        (align (fillCat 
                               (map (\is -> encloseSep' lbrace rbrace "," (int (length is):map int is) <$$> ",") 
                                  inputs)))
               <> semi
       , "int" <+> "result[TESTS_SIZE]" <+> "="
               <+> enclose lbrace rbrace
                     (align (fillCat (intersperse "," $ map (int.fromEnum) shouldResult)))
               <> semi
       , "int" <+> "should_match[TESTS_SIZE]" <+> "="
               <+> enclose lbrace rbrace
                     (align (fillCat (intersperse "," $ map (int.fromEnum) shouldMatch)))
               <> semi
       , "int" <+> "should_consume[TESTS_SIZE]" <+> "="
               <+> enclose lbrace rbrace
                     (align (fillCat (intersperse "," $ map int shouldConsume)))
               <> semi
       , ctp   <+> "should_value[TESTS_SIZE]" <+> "="
               <+> encloseSep' lbrace rbrace ", " shouldValue <> semi

       , "static" <+> ctp <+> "*" <+> "current_value"   <+> "=" <+> "0" <> semi
       , "static int current_matched  = 0;"
       , "static int current_consumed = 0;"
       , "static int input_idx        = 0;"
       , "static int input_size       = 0;"
       , "static int * input          = 0;"
       , function uint8_t "feed_input" ["void * cc"] $ vcat
           [ "if" <+> parens ("input_idx > input_size") <+> "return 0;"
           , "return input[input_idx++];"
           ]
       , function "int" "has_more" ["void * cc"] $ vcat
           [ "return (input_idx <= input_size);" ]
       , function "void *"   "dump_output" [ "void * cc", ctp <+> "* result", "int consumed", "int exact"] $ vcat
           [ "current_value = result;"
           , "current_matched = exact;"
           , "current_consumed = consumed;"
           , "return result;"
           ]
       , function "int" "main" ["int argc", "char *argv[]"] $ vcat
           [ "int i = 0"     <> semi
           , radixTrieClb <> "_t" <+> "cb" <+> "="
                <+> encloseSep lbrace rbrace "," ["has_more", "feed_input", "dump_output" ] <> semi
           , for_ "i=0" "i<TESTS_SIZE" "i++" $ vcat
               [ "input_idx  = 1" <> semi
               , "input_size = inputs[i][0]" <> semi
               , "input      = inputs[i]"    <> semi
               , "void * current_result  =" <+> radixTrie <> "(0, &cb);"
               , if_ "result[i] == !current_result" $ vcat
                    [ "printf(\"%i: [Error: wrong result %i, should be %i]\\n\",i,!!current_result,result[i]);"
                    , "continue;"
                    ]
               , if_ "current_result" $ vcat 
                   [ if_ "current_matched != should_match[i]" $ vcat
                      [ "printf(\"%i: [Error: wrong matched %i, should be %i]\\n\",i,current_matched,should_match[i]);"
                      ]
                   , if_ "current_consumed != should_consume[i]" $ vcat
                      [ "printf(\"%i: [Error: wrong consumed %i, should be %i]\\n\",i,current_consumed,should_consume[i]);"
                      ]
                   , if_ ("memcmp(&should_value[i], current_value, sizeof(" <> ctp <>"))") $ vcat
                      [ "printf(\"%i: [Error: values not match]]\\n\",i);"
                      ]
                   ]
               ]
           , "printf(\"%i tests was run\\n\",TESTS_SIZE);"
           ]
       ]
       where (inputs, results) = unzip tests
             (shouldResultM, shouldMatch, shouldConsume) = unzip3 results
             shouldResult = map isJust shouldResultM
             shouldValue  = map (maybe "{0}" cshow) shouldResultM
