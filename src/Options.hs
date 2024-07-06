{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Options where

import qualified Control.Monad.Combinators.NonEmpty as NE
import Data.Foldable (foldl)
import Data.List.Extra (mconcatMap)
import qualified Data.List.NonEmpty as NL
import qualified Data.NonEmptyText as NT
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy.Builder as TLB
import Import
import Options.Applicative as OA hiding (action)
import Options.Applicative.NonEmpty as OA
import qualified RIO.Text as T
import System.Path as Path (AbsRelFile)
import qualified System.Path as Path
import Text.Megaparsec as M
import Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer as M

optionParser :: Parser Options
optionParser =
    Options
        <$> verbose
        <*> quiet
        <*> execute
        <*> actions
        <*> paths

alternative :: (Foldable t, Alternative a) => t (a b) -> a b
alternative = foldl (<|>) empty

verbose :: Parser Bool
verbose =
    switch
        . mconcat
        $ [ long "verbose"
          , short 'v'
          , help "Verbose output?"
          ]

quiet :: Parser Bool
quiet =
    switch
        . mconcat
        $ [ long "quiet"
          , short 'q'
          , help "Quiet mode?"
          ]

execute :: Parser Bool
execute =
    switch
        . mconcat
        $ [ long "execute"
          , short 'x'
          , help "Actually perform the rename (otherwise no updates will be done)"
          ]

paths :: Parser (NonEmpty AbsRelFile)
paths =
    fmap NL.sort
        . OA.some1
        . argument (eitherReader Path.parse)
        $ metavar "PATH(s)"

actions :: Parser (NonEmpty Action)
actions = OA.some1 action

action :: Parser Action
action =
    alternative
        [ append
        , prepend
        , lowercase
        , uppercase
        , setTo
        , regex
        , replace
        , subdir
        ]

mpReader :: MpParser a -> ReadM a
mpReader parser =
    eitherReader $ first displayException . M.parse parser "" . T.pack

mpOption :: MpParser a -> Mod OptionFields a -> Parser a
mpOption = OA.option . mpReader

append :: Parser Action
append =
    alternative
        [ appendTo BaseName
            . mpOption autonum
            . mconcat
            $ [ long "append-autonum"
              , short 'a'
              , metavar "[START]/[INCREMENT]/[PAD]"
              , help "Append an autonumber. See --prepend options for details."
              ]
        , appendTo Extension
            . mpOption autonum
            . mconcat
            $ [ long "append-autonum-ext"
              , metavar "[START]/[INCREMENT]/[PAD]"
              , internal
              ]
        , appendTo FullName
            . mpOption autonum
            . mconcat
            $ [ long "append-autonum-full"
              , metavar "[START]/[INCREMENT]/[PAD]"
              , internal
              ]
        ]
  where
    appendTo = fmap . (. Append)

prepend :: Parser Action
prepend =
    alternative
        [ prependTo BaseName
            . mpOption autonum
            . mconcat
            $ [ long "prepend-autonum"
              , short 'p'
              , metavar "[START]/[INCREMENT]/[PAD]"
              , help
                    "Prepend an autonumber starting from START, \
                    \ increasing by INCREMENT, and zero padded to PAD digits. \
                    \ If omitted, START=1, INCREMENT=1, PAD=0 by default."
              ]
        , prependTo Extension
            . mpOption autonum
            . mconcat
            $ [ long "prepend-autonum-ext"
              , metavar "[START]/[INCREMENT]/[PAD]"
              , internal
              ]
        , prependTo FullName
            . mpOption autonum
            . mconcat
            $ [ long "prepend-autonum-full"
              , metavar "[START]/[INCREMENT]/[PAD]"
              , internal
              ]
        ]
  where
    prependTo = fmap . (. Prepend)

type MpParser = Parsec Void Text

autonum :: MpParser DataTag
autonum = do
    start <- fromMaybe 1 <$> optional decimal
    inc <- fmap (fromMaybe 1) $ slash >> optional decimal
    pad <- fmap (fromMaybe 0) $ slash >> optional decimal
    eof
    return $ NumTag $ AutoNum start inc pad
  where
    slash = char '/'

lowercase :: Parser Action
lowercase =
    alternative
        [ flag' (BaseName Lowercase)
            . mconcat
            $ [ long "lowercase"
              , short 'l'
              , help "Convert to lowercase"
              ]
        , flag' (Extension Lowercase)
            . mconcat
            $ [ long "lowercase-ext"
              , internal
              ]
        , flag' (FullName Lowercase)
            . mconcat
            $ [ long "lowercase-full"
              , internal
              ]
        ]

uppercase :: Parser Action
uppercase =
    alternative
        [ flag' (BaseName Uppercase)
            . mconcat
            $ [ long "uppercase"
              , short 'u'
              , help "Convert to uppercase"
              ]
        , flag' (Extension Uppercase)
            . mconcat
            $ [ long "uppercase-ext"
              , internal
              ]
        , flag' (BaseName Uppercase)
            . mconcat
            $ [ long "uppercase-fullname"
              , internal
              ]
        ]

setTo :: Parser Action
setTo =
    alternative
        [ fmap (BaseName . SetTo)
            . strOption
            . mconcat
            $ [ long "set"
              , short 's'
              , metavar "VALUE"
              , help "Set the name to a given value"
              ]
        , fmap (Extension . SetTo)
            . strOption
            . mconcat
            $ [ long "set-ext"
              , internal
              ]
        , fmap (FullName . SetTo)
            . strOption
            . mconcat
            $ [ long "set-full"
              , internal
              ]
        ]

regex :: Parser Action
regex =
    alternative
        [ fmap (construct BaseName)
            . mpOption sed
            . mconcat
            $ [ long "regex"
              , short 'R'
              , metavar "/REGEX/REPLACE/[g][i]"
              , help
                    "Find and replace using regular expression (extended POSIX). \
                    \ \\0 to \\9 in REPLACE are back references. See '--replace' \
                    \ for trailing flags."
              ]
        , fmap (construct Extension)
            . mpOption sed
            . mconcat
            $ [ long "regex-ext"
              , internal
              ]
        , fmap (construct FullName)
            . mpOption sed
            . mconcat
            $ [ long "regex-full"
              , internal
              ]
        ]
  where
    construct target (pattern, replacement, occurence, caseSensitivity) =
        target $ RegEx pattern replacement occurence caseSensitivity

replace :: Parser Action
replace =
    alternative
        [ fmap (construct BaseName)
            . mpOption sed
            . mconcat
            $ [ long "replace"
              , short 'r'
              , metavar "/MATCH/REPLACE/[g][i]"
              , help
                    "Find and replace. 'g' flag means to replace ALL occurence \
                    \ instead of the first one. 'i' flag means to ignore \
                    \ upper/lowercase."
              ]
        , fmap (construct Extension)
            . mpOption sed
            . mconcat
            $ [ long "replace-ext"
              , internal
              ]
        , fmap (construct FullName)
            . mpOption sed
            . mconcat
            $ [ long "replace-full"
              , internal
              ]
        ]
  where
    construct target (pattern, replacement, occurence, caseSensitivity) =
        target $ Replace pattern replacement occurence caseSensitivity

sed :: MpParser (NT.NonEmptyText, Text, Occurence, CaseSensitivity)
sed = do
    delim <- satisfy $ \c -> c /= backslash && c /= newline
    let literal = fmap Right $ satisfy $ \c -> c /= delim && c /= backslash
        escaped = do
            next <- char backslash >> anySingle
            return
                $ if next == delim || next == backslash
                    then Right next
                    else Left next
        nonDelim = literal <|> escaped
    patHead :| (mconcatMap toBuilder -> patTail) <- NE.some nonDelim
    _ <- char delim
    replacement <- mconcatMap toBuilder <$> M.many nonDelim
    _ <- char delim
    flags <- M.many . satisfy $ \c -> c == 'g' || c == 'i'
    eof
    let pattern = toNonEmptyText delim patHead patTail
        occurence =
            if 'g' `elem` flags
                then AllOccurences
                else FirstOccurence
        caseSensitivity =
            if 'i' `elem` flags
                then CaseInsensitive
                else CaseSensitive
    return
        ( pattern
        , cs $ TLB.toLazyText replacement
        , occurence
        , caseSensitivity
        )
  where
    backslash = '\\'
    newline = '\n'
    toBuilder = \case
        Right ch -> TLB.singleton ch
        Left ch -> TLB.singleton backslash <> TLB.singleton ch
    toNonEmptyText delim patHead patTail = convert $ case patHead of
        Right ch -> (ch, patTail)
        Left ch -> (delim, TLB.singleton ch <> patTail)
      where
        convert (ch, tail) = NT.new ch $ cs $ TLB.toLazyText tail

subdir :: Parser Action
subdir =
    fmap SubDir
        . OA.option relDirReader
        . mconcat
        $ [ long "subdir"
          , short 'd'
          , metavar "DIR"
          , help "Move to subdirectory"
          ]
  where
    relDirReader =
        maybeReader
            $ fmap Path.dirFromFile
            . Path.fileFromDir
            . Path.path
