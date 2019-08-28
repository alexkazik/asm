{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Core.PrettyPrint.PPSM
  ( PrettyPrintState (PrettyPrintState)
  , PPSM
  , addSourceLine
  , addFirstSourceLine
  , getSourceLines
  , getSimplifiedSorceLines
  ) where

import           Asm.Core.Prelude
import qualified Data.Map.Strict                  as M

import           Asm.Core.PrettyPrint.PrettyPrint
import           Asm.Core.SourcePos.Type

data PrettyPrintState
  = PrettyPrintState
    { ppsPrevLine    :: Maybe (FilePath, Int)
    , ppsCurrentLine :: Location
    }

type PPSM = State PrettyPrintState

addSourceLine :: SourcePos -> PPSM ()
addSourceLine sp = modify (\s -> s{ppsCurrentLine=sp : ppsCurrentLine s})

addFirstSourceLine :: Location -> PPSM ()
addFirstSourceLine (sp:_) = addSourceLine sp
addFirstSourceLine _      = return ()

getSimplifiedSorceLines :: PrettyPrintState -> Map FilePath [Int]
getSimplifiedSorceLines s = foldr (\sp m -> M.alter (\a -> Just $ ordNub $ sourceLine sp : fromMaybe [] a) (sourceName sp) m) M.empty (ppsCurrentLine s)

getSourceLines :: PPSM Doc
getSourceLines = do
  s <- get
  let
    slines = getSimplifiedSorceLines s
    commonLine =
      case M.toList slines of
        [(n,[l])] -> Just (n, l)
        _         -> Nothing
    renderSP =
      if M.null slines || linesMatch (ppsPrevLine s) commonLine
        then mempty
        else "  -- SP: " ++ pshow slines
  put (PrettyPrintState commonLine [])
  return renderSP
  where
    linesMatch (Just (an, al)) (Just (bn, bl)) =
         an == bn
      && al <= bl
      && bl <= al + 10
    linesMatch _ _ = False
