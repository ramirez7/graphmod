{-# LANGUAGE DeriveGeneric #-}

module Graphmod.LookingGlass where

import GHC.Generics
import Data.Map (Map)

data Node = Node
  { label :: String
  } deriving (Show, Generic)

type NodeId = String
data Edge = Edge
  { from :: NodeId
  , to :: NodeId
  } deriving (Show, Generic)

data GraphDef = GraphDef
  { title :: String
  , nodes :: Map NodeId Node
  , edges :: [Edge]
  } deriving (Show, Generic)
          
