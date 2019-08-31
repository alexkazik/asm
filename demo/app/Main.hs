{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE NoQuasiQuotes #-}

module Main
  ( main
  , runAll
  ) where

import           Control.Monad           (forM_, unless)
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy    as BL
import           Data.Semigroup          ((<>))
import           Options.Applicative     (Parser)
import qualified Options.Applicative     as O
import           System.Directory        (doesDirectoryExist)
import           System.FilePath         ((</>))

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
      O.strOption
        (  O.long "dir"
        <> O.short 'd'
        <> O.metavar "DIRECTORY"
        <> O.help "Target directory for all compiled modules"
        )

main :: IO ()
main = do
  options <- O.execParser $
    O.info
      (optionsParser O.<**> O.helper)
      (  O.fullDesc
      <> O.progDesc "Compile some demo modules"
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
