-- can't move this to *cabal since it will break ghci when having this in the same package as the library
{-# LANGUAGE ImplicitPrelude #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE NoQuasiQuotes #-}

module Main where

import           Control.Monad
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BSL
import           Data.Semigroup          ((<>))
import           Options.Applicative
import           System.Directory
import           System.FilePath

import           Demo.C64.Image

data Options
  = Options
    { optionsOutput  :: FilePath
    }

optionsParser :: Parser Options
optionsParser =
  Options
    <$>
      strOption
        (  long "dir"
        <> short 'd'
        <> metavar "DIRECTORY"
        <> help "Target directory for all compiled modules"
        )

main :: IO ()
main = do
  options <- execParser $
    info
      (optionsParser <**> helper)
      (  fullDesc
      <> progDesc "Compile some demo modules"
      )
  runAll (optionsOutput options)

runAll :: FilePath -> IO ()
runAll directory = do
  outputExists <- doesDirectoryExist directory
  unless outputExists $ error "Error: output directory does not exists"
  let
    moduleResults =
      [ moduleC64Image
      ]
  forM_ (concat $ map snd moduleResults) $ \(fileName, fileData) ->
    BSL.writeFile (directory </> fileName) (toLazyByteString fileData)
  putStr (concat $ map fst moduleResults)