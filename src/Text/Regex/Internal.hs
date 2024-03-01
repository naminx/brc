{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Text.Regex.Internal where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, ord)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Text.Regex.TDFA
import Text.Regex.TDFA.NewDFA.Uncons
import Prelude


class (RegexLike Regex s, Uncons s, Semigroup (BuilderType s)) => RegexData s where
    type BuilderType s
    isNull :: s -> Bool
    addR :: s -> BuilderType s
    run :: BuilderType s -> s


instance RegexData String where
    type BuilderType String = TL.Builder
    isNull = null
    addR = TL.fromString
    run = TL.unpack . TL.toLazyText


instance RegexData T.Text where
    type BuilderType T.Text = TL.Builder
    isNull = T.null
    addR = TL.fromText
    run = TL.toStrict . TL.toLazyText


instance RegexData TL.Text where
    type BuilderType TL.Text = TL.Builder
    isNull = TL.null
    addR = TL.fromLazyText
    run = TL.toLazyText


instance RegexData B.ByteString where
    type BuilderType B.ByteString = B.Builder
    isNull = B.null
    addR = B.byteString
    run = BL.toStrict . B.toLazyByteString


instance RegexData BL.ByteString where
    type BuilderType BL.ByteString = B.Builder
    isNull = BL.null
    addR = B.lazyByteString
    run = B.toLazyByteString


subBackRefs ::
    RegexData s =>
    [s]
    -> s
    -> BuilderType s
subBackRefs subgroups replacement =
    if isNull found
        then addR before <> addR after
        else
            addR before
                <> addR backRef
                <> subBackRefs subgroups after
  where
    backRefs = "\\\\(\\\\|[0-9])" :: T.Text
    (before, found, after, listSubgroups) = replacement =~ backRefs
    _dummy = before : found : after : listSubgroups
    backRef = case listSubgroups of
        [subgroup] ->
            case head2Int subgroup of
                -- This line may cause Index out of range exception
                Just subgroupNo -> subgroups !! subgroupNo
                Nothing -> subgroup
        _ -> error "Intenal error: A list of submatches should have exactly ONE element."
    head2Int ss = case uncons ss of
        Just (c, _) ->
            if isDigit c
                then Just $ ord c - ord '0'
                else Nothing
        Nothing -> Nothing
