{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-----------------------------------------------------------------------------
---- |
---- Module      :  Text.Regex
---- Copyright   :  (c) Nawamin Mukdathong 2023,
----                derived from (c) Chris Kuklewicz 2006,
----                derived from (c) The University of Glasgow 2001
---- License     :  BSD-style (see the file LICENSE)
----
---- Maintainer  :  naminx@gmail.com
---- Stability   :  experimental
---- Portability :  non-portable (regex-base needs MPTC+FD)
----
---- Regular expression matching.  Uses the POSIX regular expression
---- interface in "Text.Regex.Posix".
----
-----------------------------------------------------------------------------
--
----
---- Modified by Chris Kuklewicz to be a thin layer over the regex-posix
---- package, and moved into a regex-compat package.
----
---- Modified by Nawamin Mukdathong to operate on 'Text' type instead of
---- 'String' type. The original file is located at:
---- https://hackage.haskell.org/package/regex-compat-0.95.2.1/docs/Text-Regex.html
----
---- The original API is 'subRegex Regex input replacement'.
---- This file uses 'subRegexAll/Once regex replacement input'.
---- So you can pass 'subRegexAll/Once regex replacement :: Text -> Text' as a
---- function to substitute 'Text' using a regular expression,
---- in the same way as `sed s/regex/replacement/[g]`
---- If `regex` is `CI String`, `CI Text`, or `CI TL.Text`,
---- `subRegexAll/Once` will operate in case-insensitive mode.

module Regex (
    Regex, -- Regular expressions
    mkRegex,
    mkRegexWithOpts,
    matchRegex,
    matchRegexAll,
    subRegex,
    subRegexAll,
    subRegexOnce,
    splitRegex,
)
where

import Data.Array (Array, (!))
import Data.CaseInsensitive (CI (..))
import Data.Text.Internal (Text (..))
import qualified Data.Text.Read as T (decimal)
import RIO
import qualified RIO.Text as T (unpack)
import qualified RIO.Text.Lazy as TL (Text, unpack)
import qualified Text.Builder as TB
import Text.Regex.Base (
    RegexContext (matchM),
    RegexLike (matchAll),
    RegexMaker (makeRegexOpts, makeRegexOptsM),
    defaultCompOpt,
    defaultExecOpt,
 )
import Text.Regex.TDFA (
    CompOption,
    ExecOption,
    Regex,
    caseSensitive,
    multiline,
    newSyntax,
 )
import Types (Occurence (..))

{- | These instances allow implicit
 case-sensitive).  The syntax of regular expressions is
 otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
-}
instance RegexMaker Regex CompOption ExecOption (CI String) where
    makeRegexOptsM c e =
        makeRegexOptsM c{caseSensitive = False} e . original

instance RegexMaker Regex CompOption ExecOption (CI Text) where
    makeRegexOptsM c e =
        makeRegexOptsM c{caseSensitive = False} e . T.unpack . original

instance RegexMaker Regex CompOption ExecOption (CI TL.Text) where
    makeRegexOptsM c e =
        makeRegexOptsM c{caseSensitive = False} e . TL.unpack . original

{- | Makes a regular expression with the default options (multi-line,
 case-sensitive).  The syntax of regular expressions is
 otherwise that of @egrep@ (i.e. POSIX \"extended\" regular
 expressions).
-}
mkRegex :: (RegexMaker Regex CompOption ExecOption r) => r -> Regex
mkRegex = makeRegexOpts opt defaultExecOpt
  where
    opt = defaultCompOpt{newSyntax = True, multiline = False}

mkRegexWithOpts ::
    (RegexMaker Regex CompOption ExecOption r) =>
    -- | The regular expression to compile
    r ->
    -- | 'True' @\<=>@ @\'^\'@ and @\'$\'@ match the beginning and
    -- end of individual lines respectively, and @\'.\'@ does /not/
    -- match the newline character.
    Bool ->
    -- | 'True' @\<=>@ matching is case-sensitive
    Bool ->
    -- | Returns: the compiled regular expression
    Regex
mkRegexWithOpts s multiLine caseSensitive =
    makeRegexOpts opt defaultExecOpt s
  where
    opt =
        defaultCompOpt
            { multiline = multiLine
            , caseSensitive = caseSensitive
            , newSyntax = True
            }

-- | Match a regular expression against a string
matchRegex ::
    -- | The regular expression
    Regex ->
    -- | The string to match against
    Text ->
    -- | Returns: @'Just' strs@ if the match succeeded
    -- (and @strs@ is the list of subexpression matches),
    -- or 'Nothing' otherwise.
    Maybe [Text]
matchRegex p str = (\(_, _, _, str) -> str) <$> matchRegexAll p str

{- | Match a regular expression against a string, returning more information
 about the match.
-}
matchRegexAll ::
    -- | The regular expression
    Regex ->
    -- | The string to match against
    Text ->
    -- | Returns: 'Nothing' if the match failed, or:
    --
    -- >  Just ( everything before match,
    -- >         portion matched,
    -- >         everything after the match,
    -- >         subexpression matches )
    Maybe (Text, Text, Text, [Text])
matchRegexAll = matchM

{- | Replaces every or only first occurance of the given regexp with the
 replacement string.

 In the replacement string, @\"\\1\"@ refers to the first substring;
 @\"\\2\"@ to the second, etc; and @\"\\0\"@ to the entire match.
 @\"\\\\\\\\\"@ will insert a literal backslash.

 This does not advance if the regex matches an empty string.  This
 misfeature is here to match the behavior of the the original
 Text.Regex API.
-}
subRegexCommon ::
    -- | `FirstOccurence` or `AllOccurences`
    Occurence ->
    -- | Search pattern
    Regex ->
    -- | Replacement text
    Text ->
    -- | Input string
    Text ->
    -- | Output string
    Text
subRegexCommon _ _ _ "" = ""
subRegexCommon
    replaceTargets
    regexp
    replacement@(Text replacementArray replacementBegin replacementLength)
    input@(Text inputArray inputBegin inputLength) =
        TB.run $ go inputBegin $ case replaceTargets of
            AllOccurences -> matchResults
            FirstOccurence -> take 1 matchResults
      where
        matchResults = matchAll regexp input

        inputSubstr pos len = TB.text $ Text inputArray pos len
        inputEnd = inputBegin + inputLength

        go ::
            -- \| current input offset
            Int ->
            -- \| [firstMatch, secondMatch, ...]
            -- \| each match is an array of (offset, length) of subexpressions,
            -- \| refenced to `input`
            [Array Int (Int, Int)] ->
            TB.Builder
        go inputOffset [] = inputSubstr inputOffset $ inputEnd - inputOffset
        go inputOffset (firstMatch : nextMatches) =
            leadingUnmatch <> compiled firstMatch remaining
          where
            (matchOffset, matchLength) = firstMatch ! 0
            nextOffset = matchOffset + matchLength
            leadingUnmatch = inputSubstr inputOffset $ matchOffset - inputOffset
            remaining =
                if nextOffset >= inputEnd
                    then TB.text ""
                    else go nextOffset nextMatches

        compiled ::
            -- \| single match info, an array of (offset, length) of subexpressions,
            -- \| referenced to `input`
            Array Int (Int, Int) ->
            -- \| remaining `replacement`
            TB.Builder ->
            TB.Builder
        compiled = compile replacementBegin findrefs
          where
            -- bre matches a backslash then capture either a backslash or a single digit
            bre = mkRegex ("\\\\(\\\\|[0-9])" :: Text)
            findrefs = map (! 0) $ matchAll bre replacement

        -- \| This is needed to get `T.decimal escaped`
        replacementSubtxt = Text replacementArray
        replacementSubstr pos len = TB.text $ replacementSubtxt pos len
        replacementEnd = replacementBegin + replacementLength

        compile ::
            -- \| current replacement offset
            Int ->
            -- \| list of (position, length) of \0, \1, ..., referenced to `replacement`
            [(Int, Int)] ->
            -- \| single match info, an array of (offset, length) of subexpressions,
            -- \| referenced to `input`
            Array Int (Int, Int) ->
            -- \| remaining `replacement`
            TB.Builder ->
            TB.Builder
        compile replacementOffset [] _ remaining =
            replacementSubstr replacementOffset (replacementEnd - replacementOffset) <> remaining
        compile replacementOffset ((matchOffset, matchLength) : rest) matches remaining =
            leadingLiteralMatch <> subexpr <> nextRemaining
          where
            escaped = replacementSubtxt (matchOffset + 1) $ matchLength - 1
            nextOffset = matchOffset + matchLength
            leadingLiteralMatch = replacementSubstr replacementOffset $ matchOffset - replacementOffset
            subexpr = case T.decimal escaped of
                Right (subexprNo, _) ->
                    let (off, len) = matches ! subexprNo
                     in inputSubstr off len
                Left _ -> TB.text escaped
            nextRemaining =
                if nextOffset >= replacementEnd
                    then remaining
                    else compile nextOffset rest matches remaining

-- | Replaces every occurance of the given regexp with the replacement string.
subRegex ::
    -- | Search pattern
    Regex ->
    -- | Input string
    Text ->
    -- | Replacement text
    Text ->
    -- | Output string
    Text
subRegex = flip . subRegexCommon AllOccurences

{- | Replaces every occurance of the given regexp with the replacement string.
 The second argument can be `String`, strict `Text`
-}
subRegexAll ::
    (RegexMaker Regex CompOption ExecOption r) =>
    -- | Search pattern
    r ->
    -- | Replacement text
    Text ->
    -- | Input string
    Text ->
    -- | Output string
    Text
subRegexAll = subRegexCommon AllOccurences . mkRegex

-- | Replaces only first occurance of the given regexp with the replacement string.
subRegexOnce ::
    (RegexMaker Regex CompOption ExecOption r) =>
    -- | Search pattern
    r ->
    -- | Replacement text
    Text ->
    -- | Input string
    Text ->
    -- | Output string
    Text
subRegexOnce = subRegexCommon FirstOccurence . mkRegex

{- | Splits a string based on a regular expression.  The regular expression
 should identify one delimiter.

 This does not advance and produces an infinite list of [] if the regex
 matches an empty string.  This misfeature is here to match the
 behavior of the the original Text.Regex API.
-}
splitRegex :: Regex -> Text -> [Text]
splitRegex _ "" = []
splitRegex delim input =
    let matches = map (! 0) $ matchAll delim input
        go inputOffset [] = [Text inputArray inputOffset $ inputEnd - inputOffset]
        go inputOffset ((matchOffset, matchLength) : rest) =
            let nextOffset = matchOffset + matchLength
                firstline = Text inputArray inputOffset $ matchOffset - inputOffset
             in seq nextOffset
                    $ if nextOffset >= inputEnd
                        then [firstline, ""]
                        else firstline : go nextOffset rest
     in go inputBegin matches
  where
    Text inputArray inputBegin inputLength = input
    inputEnd = inputBegin + inputLength
