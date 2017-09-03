{-# LANGUAGE ImplicitPrelude #-}

module Asm.Core.File
  ( getRelativeFilePathQ
  , getRelativeFilePath
  , embedFile
  , embedDir
  ) where

import           Control.Monad
import qualified Data.FileEmbed             as FE
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import           System.Directory
import           System.FilePath
import           TH.RelativePaths

getRelativeFilePathQ :: Bool -> FilePath -> TH.Q FilePath
getRelativeFilePathQ addDependentFile fp = do
  fp' <- normalise <$> case splitDirectories fp of
    (".":_) -> do -- something relative to the file it it defined in
      loc <- TH.location
      dir <- TH.runIO getCurrentDirectory
      return $ dir </> takeDirectory (TH.loc_filename loc) </> fp
    _ -> pathRelativeToCabalPackage fp
  when addDependentFile (TH.qAddDependentFile fp')
  return fp'

getRelativeFilePath :: FilePath -> TH.Q TH.Exp
getRelativeFilePath fp = TH.lift =<< getRelativeFilePathQ False fp

embedFile :: FilePath -> TH.Q TH.Exp
embedFile fp = FE.embedFile =<< getRelativeFilePathQ True fp

embedDir :: FilePath -> TH.Q TH.Exp
embedDir fp = FE.embedDir =<< getRelativeFilePathQ True fp
