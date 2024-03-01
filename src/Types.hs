{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Data.NonEmptyText
import RIO
import RIO.Process
import System.Path


data Action
  = BaseName Verb
  | Extension Verb
  | FullName Verb
  | SubDir RelDir
  deriving (Eq, Show)


data Verb
  = Append DataTag
  | Prepend DataTag
  | Lowercase
  | Uppercase
  | SetTo Replacement
  | RegEx Pattern Replacement Occurence CaseSensitivity
  | Replace Pattern Replacement Occurence CaseSensitivity
  deriving (Eq, Show)


data DataTag
  = NumTag AutoNum
  | TimeTag DateTime
  deriving (Eq, Show)


data AutoNum = AutoNum
  { start :: Int
  , inc :: Int
  , pad :: Int
  }
  deriving (Eq, Show)


newtype DateTime = DateTime Pattern
  deriving (Eq, Show)


type Pattern = NonEmptyText
type Replacement = Text


data Occurence
  = FirstOccurence
  | AllOccurences
  deriving (Eq, Show)


data CaseSensitivity
  = CaseSensitive
  | CaseInsensitive
  deriving (Eq, Show)


data Options = Options
  { verbose :: !Bool
  , quiet :: !Bool
  , execute :: !Bool
  , actions :: !(NonEmpty Action)
  , paths :: !(NonEmpty AbsRelFile)
  }


data App = App
  { logFunc :: !LogFunc
  , processContext :: !ProcessContext
  , options :: !Options
  -- Add other app-specific configuration information here
  }


data Range = Range
  { off :: Int
  , len :: Int
  }


instance HasLogFunc App where
  logFuncL = lens (.logFunc) $ \x y -> x {logFunc = y}


instance HasProcessContext App where
  processContextL = lens (.processContext) $ \x y -> x {processContext = y}
