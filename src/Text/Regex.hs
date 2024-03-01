{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-----------------------------------------------------------------------------
--
-- Modified by Chris Kuklewicz to be a thin layer over the regex-posix
-- package, and moved into a regex-compat package.
--
-- Modified by Nawamin Mukdathong to operate on 'Text' type instead of
-- 'String' type. The original file is located at:
-- https://hackage.haskell.org/package/regex-compat-tdfa-0.95.1.4/docs/src/Text-Regex.html
--

-- |
-- Module      :  Text.Regex
-- Copyright   :  (c) Nawamin Mukdathong 2023,
--                derived from (c) Chris Kuklewicz 2006,
--                derived from (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  naminx@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (regex-base needs MPTC+FD)
--
-- Regular expression matching.  Uses the POSIX regular expression
-- interface in "Text.Regex.Posix".
module Text.Regex (
    -- * Regular expressions
    Regex,
    matchRegex,
    matchRegexAll,
    mkRegex,
    mkRegexWithOpts,
    subRegex,
    splitRegex,
    RegexData (..),
) where

import Text.Regex.Internal
import Text.Regex.TDFA


-- | Makes a regular expression with the default options (multi-line,
-- case-sensitive).  The syntax of regular expressions is
-- otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-- expressions).
mkRegex :: RegexMaker Regex CompOption ExecOption r => r -> Regex
mkRegex = makeRegexOpts opt defaultExecOpt
  where
    opt = defaultCompOpt {newSyntax = True, multiline = True}


-- | Makes a regular expression, where the multi-line and
-- case-sensitive options can be changed from the default settings.
mkRegexWithOpts ::
    RegexMaker Regex CompOption ExecOption r =>
    r
    -- ^ The regular expression to compile.
    -> Bool
    -- ^ 'True' iff @\'^\'@ and @\'$\'@ match the beginning and
    -- end of individual lines respectively, and @\'.\'@ does /not/
    -- match the newline character.
    -> Bool
    -- ^ 'True' iff matching is case-sensitive.
    -> Regex
    -- ^ Returns: the compiled regular expression.
mkRegexWithOpts s multi_line case_sensitive =
    let opt =
            defaultCompOpt
                { multiline = multi_line
                , caseSensitive = case_sensitive
                , newSyntax = True
                }
     in makeRegexOpts opt defaultExecOpt s


-- | Match a regular expression against a string
matchRegex ::
    RegexLike Regex s =>
    Regex
    -- ^ The regular expression
    -> s
    -- ^ The string to match against
    -> Maybe [s]
    -- ^ Returns: @'Just' strs@ if the match succeeded
    -- (and @strs@ is the list of subgroup matches),
    -- or 'Nothing' otherwise.
matchRegex p str = (\(_, _, _, str) -> str) <$> matchRegexAll p str


-- | Match a regular expression against a string, returning more information
-- about the match.
matchRegexAll ::
    RegexLike Regex s =>
    Regex
    -- ^ The regular expression.
    -> s
    -- ^ The string to match against.
    -> Maybe (s, s, s, [s])
    -- ^ Returns: 'Nothing' if the match failed, or:
    --
    -- >  Just ( everything before match,
    -- >         portion matched,
    -- >         everything after the match,
    -- >         subexpression matches )
matchRegexAll = matchM


-- | Replaces every or only first occurance of the given regexp with the
--  replacement string.
--
--  In the replacement string, @\"\\1\"@ refers to the first substring;
--  @\"\\2\"@ to the second, etc; and @\"\\0\"@ to the entire match.
--  @\"\\\\\\\\\"@ will insert a literal backslash.
--
--  This does not advance if the regex matches an empty string.  This
--  misfeature is here to match the behavior of the the original
--  Text.Regex API.
subRegex ::
    forall s.
    RegexData s =>
    Regex
    -- ^ Search pattern
    -> s
    -- ^ Input string
    -> s
    -- ^ Replacement text
    -> s
    -- ^ Output string
subRegex regex input replacement = run (go input)
  where
    go input' =
        if isNull found
            then addR before
            else
                addR before
                    <> subBackRefs subgroups0 replacement
                    <> go after
      where
        (before, found, after, subgroups) = match regex input'
        subgroups0 = found : subgroups
        _dummy = before : found : after : subgroups


-- | Splits a string based on a regular expression.  The regular expression
--  should identify one delimiter.
--
--  This does not advance and produces an infinite list of [] if the regex
--  matches an empty string.  This misfeature is here to match the
--  behavior of the the original Text.Regex API.
splitRegex :: RegexData s => Regex -> s -> [s]
splitRegex delim input =
    case matchOnceText delim input of
        Just (before, _, after) ->
            if isNull after
                then [before, after]
                else before : splitRegex delim after
        _ -> [input]
