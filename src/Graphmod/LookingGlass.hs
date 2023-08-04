{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Graphmod.LookingGlass where

import Data.Aeson qualified as Ae
import GHC.Generics
import Data.Map (Map)

data Node = Node
  { label :: String
  , color :: Maybe String
  } deriving stock (Show, Generic)
  deriving anyclass (Ae.ToJSON)

type NodeId = String
data Edge = Edge
  { from :: NodeId
  , to :: NodeId
  } deriving (Show, Generic)
  deriving anyclass (Ae.ToJSON)

data GraphDef = GraphDef
  { title :: String
  , nodes :: Map NodeId Node
  , edges :: [Edge]
  } deriving (Show, Generic)
  deriving anyclass (Ae.ToJSON)
