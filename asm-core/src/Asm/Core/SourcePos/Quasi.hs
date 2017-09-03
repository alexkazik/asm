{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Core.SourcePos.Quasi
  ( sourcePos
  ) where

import           Asm.Core.Prelude
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote

import           Asm.Core.Flags
import           Asm.Core.PrettyPrint.Use
import           Asm.Core.SourcePos.Type

-- external definition
sourcePos  :: QuasiQuoter
sourcePos  =  QuasiQuoter
  { quoteExp = quoteSourcePos
  , quotePat = error "QuasiQuoter \"sourcePos\" for patterns is not available"
  , quoteDec = error "QuasiQuoter \"sourcePos\" for declarations is not available"
  , quoteType = error "QuasiQuoter \"sourcePos\" for types is not available"
  }

-- expression quoter
quoteSourcePos :: String -> TH.ExpQ
quoteSourcePos s
  | flagDebugCompiler = do
      pos <- getPosition
      TH.listE
        [ TH.tupE
          [ dataToExpQ (const Nothing) [pos]
          , bool (convErr s) (TH.litE (TH.StringL "SOURCE")) (null s)
          ]
        ]
quoteSourcePos "" =
  TH.listE []
quoteSourcePos s =
  TH.listE
    [ TH.tupE
      [ dataToExpQ (const Nothing) spInternal
      , convErr s
      ]
    ]

convErr :: String -> TH.ExpQ
convErr s = TH.appE (TH.varE 'concat) (TH.listE (go s))
  where
    go :: String -> [TH.ExpQ]
    go [] = []
    go ('$':s':s'')
      | s' `elem` ("abcdefghijklmnopqrstuvwxyz" :: Text) =
        let
          (a, b) = break (`notElem` ("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :: Text)) s''
        in
          TH.appE (TH.varE 'showPretty) (TH.varE $ TH.mkName (s':a)) : go b
      | otherwise = TH.litE (TH.StringL "$") : go (s':s'')
    go s' =
      let
        (a, b) = break (== '$') s'
      in
        if null a
          then go b
          else TH.litE (TH.StringL a) : go b
