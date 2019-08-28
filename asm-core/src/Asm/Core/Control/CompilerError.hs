{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm.Core.Control.CompilerError
  ( CompilerError(..)
  -- create error
  , printError
  , fromJustOrError
  -- throw and catch
  , throwFatalError
  , throwError
  , catchError
  , catchFatalError
  -- from Asm.Data.Control.Monad.Error
  , ME.Error
  , ME.runError
  , ME.recoverFatalError
  , ME.promoteErrorToFatalError
  -- internal error (without MonadError)
  , printInternalError
  ) where

import           Asm.Core.Prelude
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax             (liftData)

import           Asm.Core.Data.CompilerError
import           Asm.Core.Flags
import           Asm.Core.Phases.Data.CompilerState1234
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos.Type
import qualified Asm.Data.Control.Monad.Error           as ME


-- create error

printError :: Q Exp
printError =
  if flagDebugCompiler
    then do
      pos <- getPosition
      [e|(\err -> printCompilerError $ convertCompilerErrorWithoutState (err ++ [($(liftData [pos]), "SOURCE")]))|]
    else
      [e|printCompilerError . convertCompilerErrorWithoutState|]

fromJustOrError :: Q Exp
fromJustOrError =
  if flagDebugCompiler
    then do
      pos <- getPosition
      [e|(\err -> maybe (convertError ME.throwFatalError (err ++ [($(liftData [pos]), "SOURCE")])) return)|]
    else
      [e|(\err -> maybe (convertError ME.throwFatalError err) return)|]

-- throw and catch

throwFatalError :: Q Exp
throwFatalError =
  if flagDebugCompiler
    then do
      pos <- getPosition
      [e|(\err -> convertError ME.throwFatalError (err ++ [($(liftData [pos]), "SOURCE")]))|]
    else
      [e|convertError ME.throwFatalError|]

throwError :: Q Exp
throwError =
  if flagDebugCompiler
    then do
      pos <- getPosition
      [e|(\err -> convertError ME.throwError (err ++ [($(liftData [pos]), "SOURCE")]))|]
    else
      [e|convertError ME.throwError|]

catchError :: ME.MonadError CompilerError m => m a -> ([(Location, String)] -> m a) -> m a
catchError m h = ME.catchError m (h . fst . fromCompilerError)

catchFatalError :: ME.MonadError CompilerError m => m a -> ([(Location, String)] -> m a) -> m a
catchFatalError m h = ME.catchError m (h . fst . fromCompilerError)

-- internal error (without MonadError)

printInternalError :: QuasiQuoter
printInternalError =  QuasiQuoter
  { quoteExp = quotePrintInternalError
  , quotePat = error "QuasiQuoter \"printInternalError\" for patterns is not available"
  , quoteDec = error "QuasiQuoter \"printInternalError\" for declarations is not available"
  , quoteType = error "QuasiQuoter \"printInternalError\" for types is not available"
  }
  where
    quotePrintInternalError :: String -> Q Exp
    quotePrintInternalError err
      | flagDebugCompiler = do
          pos <- getPosition
          err' <- convErr err
          [e|printCompilerError $ convertCompilerErrorWithoutState [($(liftData [pos]), $(pure err'))]|]
      | otherwise = do
          err' <- convErr err
          [e|errorWithoutStackTrace "Error: Internal error (this should not have happened, enable DebugCompiler to trace it)" `const` ($(pure err') :: String)|]
    convErr :: String -> Q Exp
    convErr s = appE (varE 'concat) (listE (go s))
      where
        go :: String -> [Q Exp]
        go [] = []
        go ('$':s':s'')
          | s' `elem` ("abcdefghijklmnopqrstuvwxyz" :: Text) =
            let
              (a, b) = break (`notElem` ("_'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :: Text)) s''
            in
              appE (varE 'showPretty) (varE $ mkName (s':a)) : go b
          | otherwise = litE (StringL "$") : go (s':s'')
        go s' =
          let
            (a, b) = break (== '$') s'
          in
            if null a
              then go b
              else litE (StringL a) : go b

-- internal

convertError
  :: CompilerState1234 m
  => (CompilerError -> m a)
  -> [(Location, String)]
  -> m a
convertError fn err
  | flagDebugCompiler = do
      st <- dumpStateC
      fn (CompilerError (err, Just st))
  | otherwise = fn (CompilerError (err, Nothing))
