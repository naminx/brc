{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.CaseInsensitive where

import Data.NonEmptyText as NET
import Data.Text.Internal as T (Text (..), text)
import Data.Text.Internal.Search (indices)
import RIO
import qualified RIO.Char as C
import qualified RIO.Text as T


breakOn :: NonEmptyText -> Text -> (Text, Text)
breakOn (toText -> T.map C.toLower -> pat) src@(Text arr off len) =
  case indices pat $ T.map C.toLower src of
    [] -> (src, T.empty)
    (x : _) -> (text arr off x, text arr (off + x) (len - x))
{-# INLINE breakOn #-}


splitOn ::
  -- | String to split on.
  NonEmptyText ->
  -- | Input text.
  Text ->
  [Text]
splitOn
  nonEmptyPat@(toText -> T.map C.toLower -> pat@(Text _ _ l))
  src@(Text arr off len)
    | NET.length nonEmptyPat == 1 =
        T.split (\c -> C.toLower c == C.toLower (NET.head nonEmptyPat)) src
    | otherwise = go 0 (indices pat $ T.map C.toLower src)
   where
    go !s (x : xs) = text arr (s + off) (x - s) : go (x + l) xs
    go s _ = [text arr (s + off) (len - s)]
{-# INLINE [1] splitOn #-}
