-- | Copyright: (c) 2015, Alexander Vershilov
--   Author:    Alexander Vershilov <alexander.vershilov@gmail.com>
--
--  XXX: Support total alphabet
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module LCB.Generate
  ( generate 
  , generateTests
  , generateFiles
  , lookupG
  , convertTrie
  , numerate
  , normalizeValues
  , prepareNode
  ) where

import Language.C.Generate.Types
import Language.C.Generate
import           Data.TrieMap (T(..))
import           Data.TrieMap.Utils hiding (recode)

import Text.PrettyPrint.Leijen.Text
import qualified Data.Text.Lazy    as Text
import Data.Char

import qualified Control.Applicative as A
import           Data.Foldable hiding (for_)
import           Data.Traversable
import           Data.List hiding (mapAccumL)
import           Data.Maybe
import qualified Data.Monoid as M
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

prefixed :: String -> String -> String
prefixed "" x = x
prefixed c  x = c ++ '_':x

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
                                     , fmap (\x -> (`elemIndex` l) =<< x) b)) Set.empty n
        l = Set.toList s

prepareNode :: Int -> Node Int (Int, (Bool, b)) -> Node Int Doc
prepareNode sz (Node (v,(t,_)) d) = Node (encloseSep' lbrace rbrace "," (cshow v:int (toNodeType t d):go d))
                                         (node d)
  where toNodeType :: Bool -> NodeData a b -> Int
        toNodeType False NodeSwitch{} = 0
        toNodeType True  NodeSwitch{} = 1
        toNodeType False NodeChunk{}  = 2
        toNodeType True  NodeChunk{}  = 3
        go (NodeSwitch m) = [maybe (int 0) (\(Node (w,_) _) -> int w) $ i `Map.lookup` m | i  <- [0..sz]]
        go (NodeChunk l n) = int (length l):map int l ++ [maybe "0" (\(Node (w,_) _) -> int w) n]
	node (NodeSwitch m)  = NodeSwitch $ Map.map (prepareNode sz) m
        node (NodeChunk l m) = NodeChunk l $ fmap (prepareNode sz) m

generateFiles :: CShow a
              => String
	      -> String
	      -> String
              -> T Int Int
              -> [(t, (Maybe Int, Chunked Int))]
              -> [Int]
              -> [a]
              -> [[Int]]
              -> [(String, Doc)]
generateFiles p structName hdr t v a r ts = 
     [ (prefixed p "radix.c",       generate p ctp hdr v a r)
     , (prefixed p "radix.h",       generateHeader p ctp hdr)
     , (prefixed p "radix_tests.c", generateTests p ctp hdr t a r ts)
     ]
 where
   ctp :: Doc
   ctp = case ctype (head r) of
           [y] -> y
           _  | structName == "" -> error "struct option should be provided"
              | otherwise  -> string (Text.pack structName)

generate :: CShow a
         => String
         -> Doc
         -> String
         -> [(t, (Maybe Int, Chunked Int))]
         -> [Int]
         -> [a]
         -> Doc
generate p ctp hdr v a r = vcat 
     [ "#include" <+> "<stdint.h>"
     , if hdr == ""
       then empty
       else "#include" <+> (string $ Text.pack hdr)
     , "#include" <+> dquotes (string $ Text.pack $ prefixed p "radix.h")
     , "#define" <+> "CHUNK_NUM"   <+> int chunksNo
     , "#define" <+> "ALPHABET"    <+> int alphabetSize 
     , "#define" <+> "RESULTS_NUM" <+> int resultsSize
     , linebreak
     , encodeTbl
     , linebreak
     , "static" <+> chunkType <+> "chunks" <> brackets "CHUNK_NUM" <> brackets "ALPHABET+2" <+> "=" <+>
         enclose lbrace rbrace
           (align (fillCat $ (map (mkChunk  alphabetSize) v))) -- TODO: use nest
           <> semi
     , linebreak
     , ctp <+> "results" <> brackets "RESULTS_NUM" <+> "=" <+>
         encloseSep' lbrace rbrace ", " (map cshow r) <> semi
     , linebreak
     , function "int" (string $ Text.pack $ prefixed p "radix_trie")
                      [ "void"   <+> "*cc"
                      , (string $ Text.pack $ prefixed p "radix_trie_clb_t") <+> "*cb"
                      ] $ vcat
             [ chunkType <+> "i = 0" <> semi
             , "int consumed = 0" <> semi
	     , "int may_match = 1" <> semi
             , nest 4 ("do" <+> lbrace <$> do1) <$> rbrace <+> "while (1)" <> semi
	     , "result:"
             , "if (!chunks[i][0]) { return 0; }" <> "// no value is associated with node"
             , "cb->consume_result(cc, &results[chunks[i][0]-1], consumed, may_match & chunks[i][1] & 1)" <> semi
             ]
     ]
     where
       do1   = vcat
                 [ "may_match" <+> "=" <+> int 1 <> semi 
                 , chunkType <+> "next = 0" <> semi
                 , uint8_t <+> "s" <+> "=" <+> int 0 <> semi
                 , "switch(chunks[i][1])" <>
                     block (vcat
                       [ "case 0:"
                       , "case 1:"
                       , indent 4 $ vcat [next, nextChunk, "if (next == 0) goto result"<> semi,"++consumed"<>semi, "break;"]
                       , "case 2:"
                       , "case 3:"
                       , indent 4 $ vcat
                          [ "may_match" <+> "=" <+> int 0 <> semi 
                          , for_ "" "s < chunks[i][2]" "s++" $ vcat 
                              [ next
                              , if_ "c != chunks[i][3+s]" $ "goto result" <> semi
                              , "++consumed" <> semi
                              ]
                          , "may_match" <+> "=" <+> int 1 <> semi
                          , "next = chunks[i][3+s]" <> semi
                          ]
                       ])
                 , "if (next == 0) break" <> semi
                 , "i = next" <> semi
                 ]
       chunksNo     = length v
       resultsSize  = length r
       alphabetSize = length a
       fullAlphabet = alphabetSize == alphabetMaxSize
       encodeTbl
         | fullAlphabet = empty
         | otherwise = "static" <+> uint8_t <+> "encode_tbl" <> "[]" <+> "=" <+> 
             encloseSep' lbrace rbrace "," (map int (buildAlphabet a)) <> semi
       nextChunk :: Doc
       nextChunk
         | fullAlphabet = "next = chunks[i][c]" <> semi
	 | otherwise    = "next = chunks[i][c+1]" <> semi
       next :: Doc
       next = vcat
         [ "if" <+> parens ("!" <> "cb->has_more_input" <> parens ("cc")) </> "goto result;"
         , encode
         ]
       encode :: Doc
       encode
         | fullAlphabet = uint8_t <+> "c" <+> "=" <+> "cb->get_input(cc)" <> semi
         | otherwise    = vcat 
            [ uint8_t <+> "c" <+> "=" <+> "encode_tbl[cb->get_input(cc)]" <> semi
            , nest 4 ("if (c == 0)" </> "goto result" <> semi)
            ]
       chunkType = findMaxType (length v)


generateHeader :: String -> Doc -> String -> Doc
generateHeader p ctp hdr = vcat
    [ "#ifndef" <+> (string $ Text.pack $ prefixed (map toUpper p) "RADIX_TREE_H")
    , "#define" <+> (string $ Text.pack $ prefixed (map toUpper p) "RADIX_TREE_H")
    , "#include" <+> "<stdint.h>"
    , if hdr == ""
      then empty
      else "#include" <+> (string $ Text.pack hdr)
    , linebreak
    , "typedef" <+> "struct" <+> radix_trie_clb <>
        nest 4 (lbrace <$> vcat
                       [ "int"    <+> parens ("*" <> "has_more_input") <+> parens ("void *") <> semi
                       , uint8_t  <+> parens ("*" <> "get_input") <+> parens ("void *") <> semi
                       , "int"    <+> parens ("*" <> "consume_result")
                                  <+> tupled [ "void *"
                                             , ctp <+> "*"
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

generateTests :: CShow a
              => String
              -> Doc
              -> String
              -> T Int Int
              -> [Int]
              -> [a]
              -> [[Int]]
              -> Doc
generateTests p ctp hdr t a v inputs = vcat
    [ "#include <stdint.h>" 
    , "#include <stdlib.h>"
    , "#include <stdio.h>"
    , if hdr == ""
      then empty
      else "#include" <+> (string $ Text.pack hdr)
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
    , ctp <+> "should_value[TESTS_SIZE]" <+> "=" <+>
        encloseSep' lbrace rbrace ", " (map (\i -> cshow (v!!i)) values) <> semi
    , linebreak
    , "static" <+> ctp <+> "*" <+> "current_value"   <+> "=" <+> "NULL" <> semi
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
    , function "int"   "dump_output" [ "void * cc", ctp <+> "* result", "int consumed", "int exact"] $ vcat
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
                    , if_ ("memcmp(&should_value[i], current_value, sizeof(" <> ctp <>"))") $ vcat
                        [ "printf(\"%i: [Error: values not match]]\\n\",i)" <> semi
                        , "continue" <> semi
                        ]
                    ]
              , "printf(\"%i: OK\\n\", i)" <> semi
              ]) <$> rbrace
        ]
    ]
  where
    (results, matched, consumed, values) = unzip4 $ map (lookupG t . recode) inputs
    recode
      | length a == alphabetMaxSize = id
      | otherwise      = map (\i -> fromJust $ i `elemIndex` ((-1):a))    


lookupG :: (T Int Int) -> [Int] -> (Int, Int, Int, Int)
lookupG = go 0 
  where go c (T v m) [] 
            = (fromEnum (isJust v), fromEnum (Map.null m), c, fromMaybe 0 v)
	go c (T _ m) ((\x -> x `Map.lookup` m -> Just t) :xs)
	    = go (c+1) t xs
	go c (T v _) _
	    = (fromEnum (isJust v), 0, c, fromMaybe 0 v)


buildAlphabet :: [Int] -> [Int]
buildAlphabet = go 0 1
  where
    go c v (x:xs)
      | c == alphabetMaxSize = []
      | c == x    = v:go (c+1) (v+1) xs
      | otherwise = 0:go (c+1) v     (x:xs)
    go c _ []     = replicate (alphabetMaxSize - c) 0

commonChunk, terminalChunk, listChunk, listTerminalChunk :: Doc
commonChunk       = int 0
terminalChunk     = int 1
listChunk         = int 2
listTerminalChunk = int 3

mkChunk :: Int
        -> (t, (Maybe Int, Chunked Int))
        -> Doc
mkChunk k (_,(i, Left (mnext, lst))) = encloseSep' lbrace rbrace "," (int i':typ:sz:map int lst') <$$> ","
  where
    i'   = maybe 0 succ i
    next = maybe 0 id mnext
    lst' = take (k-1) $ lst ++ next:repeat 0
    sz = int (length lst)
    typ 
      | isNothing mnext = listTerminalChunk 
      | otherwise       = listChunk
mkChunk k (_,(i, Right m)) = encloseSep' lbrace rbrace "," (int i':typ:map int lst) <$$> ","
  where
    lst = [fromMaybe 0 (Map.lookup j m) | j <- [1..k]]
    i'  = maybe 0 succ i
    typ 
      | Map.null m = terminalChunk 
      | otherwise  = commonChunk 
