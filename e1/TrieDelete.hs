module TrieDelete where

import           TrieDef

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

trieDelete :: [Char] -> Trie a -> Trie a
trieDelete str (TrieNode val children) = newTrie
  where newTrie
          | trieIsEmpty (TrieNode val children) = TrieNode Nothing Map.empty
          | str == "" = TrieNode Nothing children
          | otherwise = TrieNode val (Map.fromList (filter excludeEmptyTrie (map childDelete (Map.toList children))))
          where childDelete (childKey, childTrie)
                  | head str == childKey = (childKey, trieDelete (tail str) childTrie)
                  | otherwise = (childKey, childTrie)
                excludeEmptyTrie (childKey, childTrie) = not (trieIsEmpty childTrie)
