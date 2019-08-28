{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Asm.Parser.BuildTree
  ( buildTree
  ) where

import           Asm.Core.Prelude
import           Control.Monad.Fail           (MonadFail)
import           Language.Haskell.TH          (Exp)
import qualified Language.Haskell.TH          as TH

import           Asm.Core.SourcePos

import           Asm.Parser.Data.LabelIdValue
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt


buildTree :: MonadFail m => [PStmt ps pe] -> m [PStmt ps pe]
-- if
buildTree ((loc, PSBuildIf cond):stream)            = buildTree =<< buildBlockIf loc cond [] [] stream
buildTree ((loc, PSBuildElseif _):_)                = fail $ "Unexpected elseif: " ++ sourcePosPretty loc
buildTree ((loc, PSBuildElse):_)                    = fail $ "Unexpected else: " ++ sourcePosPretty loc
buildTree ((loc, PSBuildEndif):_)                   = fail $ "Unexpected endif: " ++ sourcePosPretty loc
-- direct if
buildTree ((loc, PSBuildDirectIf cond):stream)      = buildTree =<< buildBlockDirectIf loc cond [] [] stream
buildTree ((loc, PSBuildDirectElseif _):_)          = fail $ "Unexpected $elseif: " ++ sourcePosPretty loc
buildTree ((loc, PSBuildDirectElse):_)              = fail $ "Unexpected $else: " ++ sourcePosPretty loc
buildTree ((loc, PSBuildDirectEndif):_)             = fail $ "Unexpected $endif: " ++ sourcePosPretty loc
-- block
buildTree ((loc, PSBuildNamespace na):stream)       = buildTree =<< buildNamespace loc na [] stream
buildTree ((loc, PSBuildBlock na po):stream)        = buildTree =<< buildBlock loc na po [] stream
buildTree ((loc, PSBuildFor na fr cp to st):stream) = buildTree =<< buildFor loc na fr cp to st [] stream
buildTree ((loc, PSBuildEnd):_)                     = fail $ "Unexpected end: " ++ sourcePosPretty loc
-- otherwise
buildTree ((loc, stmt):stream)                      = (:) (loc, stmt) <$> buildTree stream
buildTree []                                        = return []


buildBlockIf :: MonadFail m => SourcePos -> PExpr pe -> [PStmt ps pe] -> [(PExpr pe, [PStmt ps pe])] -> [PStmt ps pe] -> m [PStmt ps pe]
-- if
buildBlockIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildIf cond):stream) = buildBlockIf thisLoc thisCond thisStmt allStmts =<< buildBlockIf loc cond [] [] stream
buildBlockIf thisLoc thisCond thisStmt allStmts ((_loc, PSBuildElseif cond):stream) = buildBlockIf thisLoc cond [] (allStmts ++ [(thisCond, thisStmt)]) stream
buildBlockIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildElse):stream) = buildBlockIf thisLoc (loc, PEConstBool True) [] (allStmts ++ [(thisCond, thisStmt)]) stream
buildBlockIf thisLoc thisCond thisStmt allStmts ((_loc, PSBuildEndif):stream) = return $ (thisLoc, PSIfBlock $ allStmts ++ [(thisCond, thisStmt)]) : stream
-- direct if
buildBlockIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildDirectIf cond):stream) = buildBlockIf thisLoc thisCond thisStmt allStmts =<< buildBlockDirectIf loc cond [] [] stream
buildBlockIf _thisLoc _ _ _ ((loc, PSBuildDirectElseif _):_) = fail $ "Unexpected $elseif: " ++ sourcePosPretty loc
buildBlockIf _thisLoc _ _ _ ((loc, PSBuildDirectElse):_) = fail $ "Unexpected $else: " ++ sourcePosPretty loc
buildBlockIf _thisLoc _ _ _ ((loc, PSBuildDirectEndif):_) = fail $ "Unexpected $endif: " ++ sourcePosPretty loc
-- block
buildBlockIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildNamespace na):stream) = buildBlockIf thisLoc thisCond thisStmt allStmts =<< buildNamespace loc na [] stream
buildBlockIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildBlock na po):stream) = buildBlockIf thisLoc thisCond thisStmt allStmts =<< buildBlock loc na po [] stream
buildBlockIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildFor na fr cp to st):stream) = buildBlockIf thisLoc thisCond thisStmt allStmts =<< buildFor loc na fr cp to st [] stream
buildBlockIf _thisLoc _ _ _ ((loc, PSBuildEnd):_) = fail $ "Unexpected end: " ++ sourcePosPretty loc
-- otherwise
buildBlockIf thisLoc thisCond thisStmt allStmts (locstmt:stream) = buildBlockIf thisLoc thisCond (thisStmt ++ [locstmt]) allStmts stream
buildBlockIf thisLoc _ _ _ [] = fail $ "Missing endif for: " ++ sourcePosPretty thisLoc


buildBlockDirectIf :: MonadFail m => SourcePos -> Exp -> [PStmt ps pe] -> [(Exp, [PStmt ps pe])] -> [PStmt ps pe] -> m [PStmt ps pe]
-- if
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildIf cond):stream) = buildBlockDirectIf thisLoc thisCond thisStmt allStmts =<< buildBlockIf loc cond [] [] stream
buildBlockDirectIf _thisLoc _ _ _ ((loc, PSBuildElseif _):_) = fail $ "Unexpected elseif: " ++ sourcePosPretty loc
buildBlockDirectIf _thisLoc _ _ _ ((loc, PSBuildElse):_) = fail $ "Unexpected else: " ++ sourcePosPretty loc
buildBlockDirectIf _thisLoc _ _ _ ((loc, PSBuildEndif):_) = fail $ "Unexpected endif: " ++ sourcePosPretty loc
-- direct if
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildDirectIf cond):stream) = buildBlockDirectIf thisLoc thisCond thisStmt allStmts =<< buildBlockDirectIf loc cond [] [] stream
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((_loc, PSBuildDirectElseif cond):stream) = buildBlockDirectIf thisLoc cond [] (allStmts ++ [(thisCond, thisStmt)]) stream
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((_loc, PSBuildDirectElse):stream) = buildBlockDirectIf thisLoc (TH.ConE 'True) [] (allStmts ++ [(thisCond, thisStmt)]) stream
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((_loc, PSBuildDirectEndif):stream) = return $ (thisLoc, PSAntiBuildDirectIfBlock $ allStmts ++ [(thisCond, thisStmt)]) : stream
-- block
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildNamespace na):stream) = buildBlockDirectIf thisLoc thisCond thisStmt allStmts =<< buildNamespace loc na [] stream
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildBlock na po):stream) = buildBlockDirectIf thisLoc thisCond thisStmt allStmts =<< buildBlock loc na po [] stream
buildBlockDirectIf thisLoc thisCond thisStmt allStmts ((loc, PSBuildFor na fr cp to st):stream) = buildBlockDirectIf thisLoc thisCond thisStmt allStmts =<< buildFor loc na fr cp to st [] stream
buildBlockDirectIf _thisLoc _ _ _ ((loc, PSBuildEnd):_) = fail $ "Unexpected end: " ++ sourcePosPretty loc
-- otherwise
buildBlockDirectIf thisLoc thisCond thisStmt allStmts (locstmt:stream) = buildBlockDirectIf thisLoc thisCond (thisStmt ++ [locstmt]) allStmts stream
buildBlockDirectIf thisLoc _ _ _ [] = fail $ "Missing $endif for: " ++ sourcePosPretty thisLoc


buildNamespace :: MonadFail m => SourcePos -> Maybe LabelIdValue -> [PStmt ps pe] -> [PStmt ps pe] -> m [PStmt ps pe]
-- if
buildNamespace thisLoc n thisStmt ((loc, PSBuildIf cond):stream) = buildNamespace thisLoc n thisStmt =<< buildBlockIf loc cond [] [] stream
buildNamespace _thisLoc _ _ ((loc, PSBuildElseif _):_) = fail $ "Unexpected elseif: " ++ sourcePosPretty loc
buildNamespace _thisLoc _ _ ((loc, PSBuildElse):_) = fail $ "Unexpected else: " ++ sourcePosPretty loc
buildNamespace _thisLoc _ _ ((loc, PSBuildEndif):_) = fail $ "Unexpected endif: " ++ sourcePosPretty loc
-- haskell if
buildNamespace thisLoc n thisStmt ((loc, PSBuildDirectIf cond):stream) = buildNamespace thisLoc n thisStmt =<< buildBlockDirectIf loc cond [] [] stream
buildNamespace _thisLoc _ _ ((loc, PSBuildDirectElseif _):_) = fail $ "Unexpected $elseif: " ++ sourcePosPretty loc
buildNamespace _thisLoc _ _ ((loc, PSBuildDirectElse):_) = fail $ "Unexpected $else: " ++ sourcePosPretty loc
buildNamespace _thisLoc _ _ ((loc, PSBuildDirectEndif):_) = fail $ "Unexpected $endif: " ++ sourcePosPretty loc
-- block
buildNamespace thisLoc n thisStmt ((loc, PSBuildNamespace na):stream) = buildNamespace thisLoc n thisStmt =<< buildNamespace loc na [] stream
buildNamespace thisLoc n thisStmt ((loc, PSBuildBlock na po):stream) = buildNamespace thisLoc n thisStmt =<< buildBlock loc na po [] stream
buildNamespace thisLoc n thisStmt ((loc, PSBuildFor na fr cp to st):stream) = buildNamespace thisLoc n thisStmt =<< buildFor loc na fr cp to st [] stream
buildNamespace thisLoc n thisStmt ((_loc, PSBuildEnd):stream) = return $ (thisLoc, PSNamespace n thisStmt) : stream
-- otherwise
buildNamespace thisLoc n thisStmt (locstmt:stream) = buildNamespace thisLoc n (thisStmt ++ [locstmt]) stream
buildNamespace thisLoc _ _ [] = fail $ "Missing end for: " ++ sourcePosPretty thisLoc


buildBlock :: MonadFail m => SourcePos -> Maybe LabelIdValue -> Maybe (PExpr pe) -> [PStmt ps pe] -> [PStmt ps pe] -> m [PStmt ps pe]
-- if
buildBlock thisLoc n p thisStmt ((loc, PSBuildIf cond):stream) = buildBlock thisLoc n p thisStmt =<< buildBlockIf loc cond [] [] stream
buildBlock _thisLoc _ _ _ ((loc, PSBuildElseif _):_) = fail $ "Unexpected elseif: " ++ sourcePosPretty loc
buildBlock _thisLoc _ _ _ ((loc, PSBuildElse):_) = fail $ "Unexpected else: " ++ sourcePosPretty loc
buildBlock _thisLoc _ _ _ ((loc, PSBuildEndif):_) = fail $ "Unexpected endif: " ++ sourcePosPretty loc
-- haskell if
buildBlock thisLoc n p thisStmt ((loc, PSBuildDirectIf cond):stream) = buildBlock thisLoc n p thisStmt =<< buildBlockDirectIf loc cond [] [] stream
buildBlock _thisLoc _ _ _ ((loc, PSBuildDirectElseif _):_) = fail $ "Unexpected $elseif: " ++ sourcePosPretty loc
buildBlock _thisLoc _ _ _ ((loc, PSBuildDirectElse):_) = fail $ "Unexpected $else: " ++ sourcePosPretty loc
buildBlock _thisLoc _ _ _ ((loc, PSBuildDirectEndif):_) = fail $ "Unexpected $endif: " ++ sourcePosPretty loc
-- block
buildBlock thisLoc n p thisStmt ((loc, PSBuildNamespace na):stream) = buildBlock thisLoc n p thisStmt =<< buildNamespace loc na [] stream
buildBlock thisLoc n p thisStmt ((loc, PSBuildBlock na po):stream) = buildBlock thisLoc n p thisStmt =<< buildBlock loc na po [] stream
buildBlock thisLoc n p thisStmt ((loc, PSBuildFor na fr cp to st):stream) = buildBlock thisLoc n p thisStmt =<< buildFor loc na fr cp to st [] stream
buildBlock thisLoc n p thisStmt ((_loc, PSBuildEnd):stream) = return $ (thisLoc, PSBlock n p thisStmt) : stream
-- otherwise
buildBlock thisLoc n p thisStmt (locstmt:stream) = buildBlock thisLoc n p (thisStmt ++ [locstmt]) stream
buildBlock thisLoc _ _ _ [] = fail $ "Missing end for: " ++ sourcePosPretty thisLoc

buildFor :: MonadFail m => SourcePos -> LabelIdValue -> PExpr pe -> Bool -> PExpr pe -> PExpr pe -> [PStmt ps pe] -> [PStmt ps pe] -> m [PStmt ps pe]
-- if
buildFor thisLoc n f c t s thisStmt ((loc, PSBuildIf cond):stream) = buildFor thisLoc n f c t s thisStmt =<< buildBlockIf loc cond [] [] stream
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildElseif _):_) = fail $ "Unexpected elseif: " ++ sourcePosPretty loc
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildElse):_) = fail $ "Unexpected else: " ++ sourcePosPretty loc
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildEndif):_) = fail $ "Unexpected endif: " ++ sourcePosPretty loc
-- haskell if
buildFor thisLoc n f c t s thisStmt ((loc, PSBuildDirectIf cond):stream) = buildFor thisLoc n f c t s thisStmt =<< buildBlockDirectIf loc cond [] [] stream
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildDirectElseif _):_) = fail $ "Unexpected $elseif: " ++ sourcePosPretty loc
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildDirectElse):_) = fail $ "Unexpected $else: " ++ sourcePosPretty loc
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildDirectEndif):_) = fail $ "Unexpected $endif: " ++ sourcePosPretty loc
-- block
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildNamespace{}):_) = fail $ "Unexpected namespace: " ++ sourcePosPretty loc
buildFor _thisLoc _ _ _ _ _ _ ((loc, PSBuildBlock{}):_) = fail $ "Unexpected block: " ++ sourcePosPretty loc
buildFor thisLoc n f c t s thisStmt ((loc, PSBuildFor na fr cp to st):stream) = buildFor thisLoc n f c t s thisStmt =<< buildFor loc na fr cp to st [] stream
buildFor thisLoc n f c t s thisStmt ((_loc, PSBuildEnd):stream) = return $ (thisLoc, PSFor n f c t s thisStmt) : stream
-- otherwise
buildFor thisLoc n f c t s thisStmt (locstmt:stream) = buildFor thisLoc n f c t s (thisStmt ++ [locstmt]) stream
buildFor thisLoc _ _ _ _ _ _ [] = fail $ "Missing end for: " ++ sourcePosPretty thisLoc
