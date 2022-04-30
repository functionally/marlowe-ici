
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Language.Marlowe.ICI.PTree (
  PTree(..)
, insert
, printPTree
) where


import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import qualified Data.Map.Strict as M


data PTree k v =
    Empty
  | Node (M.Map k (PTree k v))
  | Leaf [v]
    deriving (Eq, Ord, Show)


printPTree :: Show k
           => Show v
           => String
           -> PTree k v
           -> IO ()
printPTree indent Empty =
  putStrLn $ indent ++ "Empty"
printPTree indent (Leaf vs) =
  putStrLn $ indent ++ show vs
printPTree indent (Node tree) =
  sequence_
    [
      do
        putStrLn $ indent ++ show k
        printPTree (indent ++ "  ") tree'
    |
      (k, tree') <- M.assocs tree
    ]


insert :: Ord k
       => Segmentable k
       => k
       -> v
       -> PTree k v
       -> PTree k v
insert key value Empty =
  if key == empty
    then Leaf [value]
    else Node . M.singleton key $ Leaf [value]
insert key value (Leaf values) =
  case segment key of
    Nothing -> Leaf $ value : values
    Just _  -> error "PTree.insert: keys have different lengths."
insert key value (Node tree) =
  if key `M.member` tree
    then Node
           . flip (M.insert key) tree
           . insert empty value
           $ tree M.! key
    else case segment key of
           Nothing               -> error "PTree.insert: keys have different lengths."
           Just (prefix, suffix) -> let
                                      segmented = segmentKeys tree
                                    in
                                      Node
                                        . flip (M.insert prefix) segmented
                                        . insert suffix value
                                        . fromMaybe Empty
                                        $ prefix `M.lookup` segmented


class Segmentable k where
  empty :: k
  composite :: k -> Bool
  segment :: k -> Maybe (k, k)


instance Segmentable String where
  empty = ""
  composite k = length k > 2
  segment k = guard (not $ null k) >> pure (splitAt 2 k)


segmentKeys :: Segmentable k
            => M.Map k (PTree k v)
            -> M.Map k (PTree k v)
segmentKeys tree =
  if M.size tree == 1
    then let
           (key, tree') = M.findMin tree
         in
           case (composite key, segment key) of
             (False, _                    ) -> tree
             (True , Just (prefix, suffix)) -> M.singleton prefix
                                                 . Node
                                                 $ M.singleton suffix tree'
             _                              -> error "PTree.segmentKeys: unexpected."
    else tree
