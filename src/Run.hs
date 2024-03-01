{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import Control.Lens (both, imap)
import Data.CaseInsensitive (mk)
import qualified Data.NonEmptyText as NT
import Data.String.Conversions (cs)
import qualified Data.Text.IO as T
import Formatting (format)
import Formatting.Combinators (lpadded)
import Formatting.Formatters (int)
import Import hiding (some, try)
import qualified Lib.CaseInsensitive as CI
import qualified RIO.Char as C
import RIO.Directory
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import RIO.Text.Partial as T (breakOn, splitOn)
import Regex
import System.Path as Path

run :: RIO App ()
run = do
    actions <- asks (NE.toList . (.options.actions))
    execute <- asks (.options.execute)
    paths <- asks (NE.toList . (.options.paths))
    result <- sequenceA $ imap (performAll actions) paths
    if execute
        then executeAll $ zip paths result
        else
            liftIO
                . traverse_ T.putStrLn
                $ zipWith (showArrow yellow) paths result
  where
    yellow = "\ESC[33m"

showArrow :: Text -> AbsRelFile -> AbsRelFile -> Text
showArrow color old new =
    mconcat
        [ gray
        , cs (Path.toString old)
        , " -> "
        , color
        , cs (Path.toString $ takeFileName new)
        , resetColor
        ]
  where
    gray = "\ESC[90m"
    resetColor = "\ESC[0m"

executeAll :: [(AbsRelFile, AbsRelFile)] -> RIO App ()
executeAll = traverse_ executeOne

executeOne :: (AbsRelFile, AbsRelFile) -> RIO App ()
executeOne (old, new) = do
    quiet <- asks (.options.quiet)
    if normalise old /= normalise new
        then do
            exist <- doesPathExist $ Path.toString new
            if exist
                then logError "Error: Destination file exists!"
                else do
                    unless quiet $ do
                        liftIO $ T.putStrLn $ showArrow green old new
                    renamePath (Path.toString old) $ Path.toString new
        else do
            unless quiet $ do
                liftIO $ T.putStrLn $ showArrow gray old new
  where
    green = "\ESC[32m"
    gray = "\ESC[90m"

performAll :: [Action] -> Int -> AbsRelFile -> RIO App AbsRelFile
performAll actions idx absRelFile =
    foldM (performOne idx) absRelFile actions

performOne :: Int -> AbsRelFile -> Action -> RIO App AbsRelFile
performOne idx absRelFile action = do
    let input = cs $ case action of
            BaseName _ -> Path.toString baseName
            Extension _ -> extension
            FullName _ -> Path.toString fullName
            SubDir _ -> Path.toString fullName
        output = cs $ case action of
            BaseName verb -> performOn idx input verb
            Extension verb -> performOn idx input verb
            FullName verb -> performOn idx input verb
            SubDir _ -> cs $ Path.toString fullName
        postProcessing =
            directory </> case action of
                BaseName _ -> mkRelFile output & addExtension $ extension
                Extension _ -> baseName & addExtension $ output
                FullName _ -> mkRelFile output
                SubDir _ -> mkRelFile output
        directory = case action of
            SubDir subDir ->
                takeDirectory absRelFile </> subDir
            _ -> takeDirectory absRelFile
    pure postProcessing
  where
    mkRelFile name = if null name then emptyFile else path name
    baseName = takeBaseName absRelFile
    extension = takeExtension absRelFile
    fullName = takeFileName absRelFile

performOn :: Int -> Text -> Verb -> Text
performOn idx input verb = case verb of
    Append (NumTag (AutoNum{start, inc, pad})) -> input <> autonum start inc pad
    Append (TimeTag (DateTime _pattern)) -> input <> undefined
    Prepend (NumTag (AutoNum{start, inc, pad})) -> autonum start inc pad <> input
    Prepend (TimeTag (DateTime _pattern)) -> undefined <> input
    Lowercase -> T.map C.toLower input
    Uppercase -> T.map C.toUpper input
    SetTo name -> name
    RegEx (NT.toText -> regexp) replacement occurence caseSensitivity ->
        -- let regex = mkRegexWithOpts regexp False $ caseSensitivity == CaseSensitive
        case (occurence, caseSensitivity) of
            (FirstOccurence, CaseSensitive) -> subRegexOnce regexp replacement input
            (FirstOccurence, CaseInsensitive) -> subRegexOnce (mk regexp) replacement input
            (AllOccurences, CaseSensitive) -> subRegexAll regexp replacement input
            (AllOccurences, CaseInsensitive) -> subRegexAll (mk regexp) replacement input
    Replace nePattern@(NT.toText -> pattern) replacement occurrence caseSensitivity ->
        T.intercalate replacement $ case (occurrence, caseSensitivity) of
            (FirstOccurence, CaseInsensitive) -> CI.breakOn nePattern input ^.. both
            (FirstOccurence, CaseSensitive) -> splitOnceOn nePattern input
            (AllOccurences, CaseInsensitive) -> CI.splitOn nePattern input
            (AllOccurences, CaseSensitive) -> T.splitOn pattern input
  where
    autonum start inc pad = cs $ format (leadingZeros pad) $ start + idx * inc
    leadingZeros pad = lpadded (fromIntegral pad) '0' int

splitOnceOn :: NT.NonEmptyText -> Text -> [Text]
splitOnceOn (NT.toText -> pat) src =
    if T.null matched
        then [prefix]
        else [prefix, suffix]
  where
    (prefix, matched) = breakOn pat src
    suffix = T.drop (T.length pat) matched
