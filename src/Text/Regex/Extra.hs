{-# LANGUAGE ImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Text.Regex.Extra (
    subRegexOnce,
    subRegexAll,
) where

import Text.Regex
import Text.Regex.Internal
import Text.Regex.TDFA
import Prelude


subRegexOnce ::
    RegexData s =>
    Regex
    -- ^ Search pattern
    -> s
    -- ^ Replacement text
    -> s
    -- ^ Input string
    -> s
    -- ^ Output string
subRegexOnce regex replacement input =
    if isNull found
        then before
        else
            run
                $ addR before
                <> subBackRefs subgroups0 replacement
                <> addR after
  where
    (before, found, after, subgroups) = match regex input
    subgroups0 = found : subgroups
    _dummy = before : found : after : subgroups


subRegexAll ::
    RegexData s =>
    Regex
    -- ^ Search pattern
    -> s
    -- ^ Replacement text
    -> s
    -- ^ Input string
    -> s
    -- ^ Output string
subRegexAll regex replacement input =
    subRegex regex input replacement
