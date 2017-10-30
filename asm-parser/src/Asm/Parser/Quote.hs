module Asm.Parser.Quote
 ( module Asm.Parser.Quote
 , getPosition
 ) where

import           Asm.Core.Prelude
import qualified Data.Text                       as T
import           Data.Typeable                   (cast)
import qualified Language.Haskell.TH             as TH
import qualified Language.Haskell.TH.Syntax      as TH

import           Asm.Core.SourcePos              (getPosition)

import           Asm.Parser.Data.Haskell
import           Asm.Parser.Data.Int64Value
import           Asm.Parser.Data.LabelIdValue
import           Asm.Parser.Data.PExpr
import           Asm.Parser.Data.PStmt
import           Asm.Parser.Data.ToArray
import           Asm.Parser.Data.ToExpr
import           Asm.Parser.Data.ToStructOrUnion
import           Asm.Parser.DirectBlock

-- helper for TH
var :: String -> TH.ExpQ
var = TH.varE . TH.mkName

con :: String -> TH.ExpQ
con = TH.conE . TH.mkName

(.*) :: TH.ExpQ -> TH.ExpQ -> TH.ExpQ
(.*) = TH.appE
infixl 1 .*

(.$) :: Data a => TH.ExpQ -> a -> TH.ExpQ
a .$ b = TH.appE a (TH.liftData b)
infixl 1 .$


-- anti quotation
antiStmt :: (Data ps, Data pe) => ([PStmt ps pe] -> TH.ExpQ) -> PStmt ps pe -> Maybe TH.ExpQ
antiStmt lft (loc, PSAntiBuildDirectIfBlock v)  = Just $ TH.varE 'Asm.Parser.DirectBlock.pickDirectBlock .$ loc .* mapDirect lft v
antiStmt _ (loc, PSAntiNamespace v n) =
  Just $
    TH.tupE
      [ TH.liftData loc
      , TH.conE 'Asm.Parser.Data.PStmt.PSTraceStep .* TH.tupE
                 [ TH.liftData loc
                 , TH.conE 'Asm.Parser.Data.PStmt.PSNamespace
                      .$ n
                      .* toExpQ v
                 ]
      ]
antiStmt _ (loc, PSAntiBlock v n p) =
  Just $
    TH.tupE
      [ TH.liftData loc
      , TH.conE 'Asm.Parser.Data.PStmt.PSTraceStep .* TH.tupE
                 [ TH.liftData loc
                 , TH.conE 'Asm.Parser.Data.PStmt.PSBlock
                      .$ n
                      .$ p
                      .* toExpQ v
                 ]
      ]
antiStmt _ _                 = Nothing

antiParsedInt64 :: Int64Value -> Maybe TH.ExpQ
antiParsedInt64 (Int64ValueHaskell e) = Just $ TH.conE 'Asm.Parser.Data.Int64Value.Int64Value .* (TH.varE 'fromIntegral .* toExpQ e)
antiParsedInt64  _       = Nothing


antiParsedLabelId :: LabelIdValue -> Maybe TH.ExpQ
antiParsedLabelId (LabelIdValueHaskell e) = Just $ TH.conE 'Asm.Parser.Data.LabelIdValue.LabelIdValue .* toExpQ e
antiParsedLabelId  _                      = Nothing

-- helper for PSAntiBuildDirectIfBlock, converts (Maybe MyExp, [Stmt]) to (HaskellExpression, [Stmt]) or (True, [Stmt])
consAntiDirect :: ([PStmt ps pe] -> TH.ExpQ) -> (Haskell, [PStmt ps pe]) -> TH.ExpQ -> TH.ExpQ
consAntiDirect liftCpu (x,y)  = (.*) (con ":" .* ne)
  where
    ne = TH.tupE [toExpQ x, liftCpu y]

mapDirect :: (Data ps, Data pe) => ([PStmt ps pe] -> TH.ExpQ) -> [(Haskell, [PStmt ps pe])] -> TH.ExpQ
mapDirect liftCpu x = foldr (consAntiDirect liftCpu) (TH.liftData ([] `asTypeOf` x)) x


-- anti quotation
antiExpr :: PExpr pe -> Maybe TH.ExpQ
antiExpr  (loc, PEAntiExpr e)   = Just $ TH.varE 'Asm.Parser.Data.ToExpr.toExpr .$ loc .* toExpQ e
antiExpr  (loc, PEAntiArray e)  = Just $ TH.varE 'Asm.Parser.Data.ToArray.toArray .$ loc .* toExpQ e
antiExpr  (loc, PEAntiStruct e) = Just $ TH.varE 'Asm.Parser.Data.ToStructOrUnion.toStructOrUnion .$ loc .* toExpQ e
antiExpr  _                     = Nothing

-- lift text
liftText :: Text -> Maybe TH.ExpQ
liftText a = (\txt -> TH.AppE (TH.VarE 'pack) <$> TH.dataToExpQ (const Nothing) (T.unpack txt)) <$> cast a
