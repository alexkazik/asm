module Asm.Parser.Data.Haskell
  ( Haskell(..)
  , toExpQ
  ) where

import           Asm.Core.Prelude
import           Data.List           (foldl1)
import           Data.Scientific
import qualified Language.Haskell.TH as TH

data Haskell
  = HVar String
  | HCon String
  | HInteger Integer
  | HScientific Scientific
  | HString String
  | HApp (NonNull [Haskell])
  | HSig String Haskell
  deriving (Eq,Ord,Show,Typeable,Data)

toExpQ :: Haskell -> TH.ExpQ
toExpQ (HVar v) = TH.varE (TH.mkName v)
toExpQ (HCon v) = TH.conE (TH.mkName v)
toExpQ (HInteger n) = TH.litE (TH.IntegerL n)
toExpQ (HScientific n) =
  case floatingOrInteger n of
    Left r  -> TH.litE (TH.RationalL $ toRational (r :: Double))
    Right i -> TH.litE (TH.IntegerL i)
toExpQ (HString s) = TH.listE (map (TH.litE . TH.CharL) s) -- explicitly a list of chars (and not a string litaral which can be overloaded)
toExpQ (HApp as) = foldl1 TH.appE (map toExpQ $ toNullable as)
toExpQ (HSig ty a) = TH.sigE (toExpQ a) (TH.conT $ TH.mkName ty)
