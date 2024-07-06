{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options
import Options.Applicative
import Options.Applicative.Simple
import Paths_brc
import RIO.Process
import Run


main :: IO ()
main = do
  options <- customExecParser (prefs showHelpOnEmpty) progInfo
  logHandler <- logOptionsHandle stderr $ options.verbose
  defaultProcessContext <- mkDefaultProcessContext
  withLogFunc logHandler $ \logFunction ->
    let app =
          App
            { logFunc = logFunction
            , processContext = defaultProcessContext
            , options = options
            }
     in runRIO app run
 where
  progInfo =
    info (optionParser <**> helper) . mconcat $
      [ fullDesc
      , header . mconcat $
          [ "Bulk Rename Command, by Namin, "
          , $(simpleVersion version)
          ]
      , progDesc
          "Rename files & directories specified by PATH(s), according to \
          \ given options in order. By default, options alter only the \
          \ base file names, leaving the file extension untouched. Where \
          \ applicable, options can be made to alter only the file \
          \ extension or the full file name by appending -ext or -full, \
          \ respectively, to their names. Ex. --replace becomes \
          \ --replace-ext or --replace-full."
      ]
