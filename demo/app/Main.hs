{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE NoQuasiQuotes #-}

module Main
  ( main
  , runAll
  ) where

import           Control.Monad
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BL
import           Data.Semigroup          ((<>))
import           Options.Applicative
import           System.Directory
import           System.FilePath

import           Demo.C64.Image
import           Demo.Multi.UnRLE
import           Demo.Output


newtype Options
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
      concat
        [ moduleC64Image
        , moduleMultiUnRLE
        ]
  forM_ (concatMap moFiles moduleResults) $ \(fileName, fileData) ->
    BL.writeFile (directory </> fileName) (toLazyByteString fileData)
  putStr (concatMap printInfo moduleResults)
  where
    printInfo mo =
      "Module " ++ moName mo ++ "\n" ++ moStats mo ++ "\n"
