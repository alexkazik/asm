module Asm.C64.Vice
  ( generateViceSybols
  ) where

import qualified Control.Arrow
import qualified Data.Map                            as M
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text.Lazy                      as TL
import           Text.Printf

import           Asm.Core
import           Asm.Core.Control.CompilerError
import           Asm.Core.Data.Reference
import           Asm.Core.Phase4.Data.CompilerResult
import           Asm.Core.Phase4.Data.CompilerState4
import           Asm.Core.Prelude

generateViceSybols :: CompilerResult c -> LText
generateViceSybols cr = intercalate "\n" (map printLab $ generateSybolTable $ M.toList $ cs4Position $ crState cr) `mappend` "\n"
  where
    printLab :: (Int64, LText) -> LText
    printLab (a, b) =
      if isInfixOf "_break_" b || isPrefixOf "break_" b || isSuffixOf "_break" b || b == "break"
        then lab `mappend` "\n" `mappend` brk
        else lab
      where
        lab = "al C:" `mappend` pack (printf "%04x" a) `mappend` " ." `mappend` b
        brk = "break ." `mappend` b

generateSybolTable :: [(Reference, (b, Either a Int64))] -> [(Int64, LText)]
generateSybolTable = sortOn fst . separateSameName . alignSameName . joinSamePos . alignSamePos
  where
    alignSamePos :: [(Reference, (b, Either a Int64))] -> [(Int64, [LText])]
    alignSamePos pos = M.toList $ foldl' go M.empty $ map (Control.Arrow.first nameOfReference) pos
      where
        go mp (n, (_, Right p)) =
          if null n''
            then mp
            else M.alter (\v -> Just $ intercalate "_" n'' : fromMaybe [] v) p mp
          where
            n' = TL.split (== '.') (TL.fromStrict n)
            n'' = filter (not . TL.any (=='~')) n'
        go _ _ = [printInternalError|generateSybolTable: not all symbols are fully defined|]
    joinSamePos :: [(Int64, [LText])] -> [(Int64, LText)]
    joinSamePos = map (\(p,t) -> (p, intercalate "_X_" (sort t)))
    alignSameName :: [(Int64, LText)] -> [(LText, [Int64])]
    alignSameName x = M.toList $ foldl' go M.empty x
      where
        go mp (p,t) = M.alter (\v -> Just $ p : fromMaybe [] v) t mp
    separateSameName :: [(LText, [Int64])] -> [(Int64, LText)]
    separateSameName ((t, [p]):xs) = (p, t) : separateSameName xs
    separateSameName ((t, ps):xs) = map (go t) (zip (sort ps) [(1::Int)..]) ++ separateSameName xs
      where
        go t' (p, n) = (p, t' `mappend` "_X" `mappend` ltshow n)
    separateSameName [] = []
