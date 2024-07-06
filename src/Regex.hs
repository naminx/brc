{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Regex (
    module Text.Regex,
    CompOption (..),
    ExecOption (..),
    RegexMaker (..),
    subRegexAll,
    subRegexOnce,
)
where

import Data.CaseInsensitive
import RIO
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import Text.Regex
import qualified Text.Regex.Extra as RX
import Text.Regex.TDFA


-- import Types (Occurence (..))

instance RegexMaker Regex CompOption ExecOption (CI String) where
    makeRegexOptsM c e =
        makeRegexOptsM c {caseSensitive = False} e . original


instance RegexMaker Regex CompOption ExecOption (CI Text) where
    makeRegexOptsM c e =
        makeRegexOptsM c {caseSensitive = False} e . T.unpack . original


instance RegexMaker Regex CompOption ExecOption (CI TL.Text) where
    makeRegexOptsM c e =
        makeRegexOptsM c {caseSensitive = False} e . TL.unpack . original


subRegexOnce ::
    (RegexMaker Regex CompOption ExecOption r, RegexData s) =>
    r
    -> s
    -> s
    -> s
subRegexOnce regex = RX.subRegexOnce $ mkRegex regex


subRegexAll ::
    (RegexMaker Regex CompOption ExecOption r, RegexData s) =>
    r
    -> s
    -> s
    -> s
subRegexAll regex = RX.subRegexAll $ mkRegex regex
