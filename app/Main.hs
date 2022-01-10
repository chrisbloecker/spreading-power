{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main
    where
--------------------------------------------------------------------------------
import Control.Monad               (forM_)
import Control.Monad.State.Strict  (evalState)
import Data.Map.Strict             (Map)
import Data.Text                   (Text)
import Options.Applicative
import Model                       (Node)
import Parser                      (parseGraph)
import SIR                         (spreadingPower, epidemicThreshold)
import Text.Megaparsec             (errorBundlePretty, parse)
import System.Random               (mkStdGen, randoms)
--------------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import qualified Data.Text.IO    as T (readFile, writeFile)
import qualified Data.Text       as T
--------------------------------------------------------------------------------

data Options = Options { inputFile  :: FilePath 
                       , outputFile :: FilePath
                       , directed   :: Bool
                       , seed       :: Int
                       , iterations :: Int
                       }

options :: Parser Options
options = Options <$> inputFile
                  <*> outputFile
                  <*> directed
                  <*> seed
                  <*> iterations
    where
        inputFile  = strOption       . mconcat $ [long "input",      metavar "INPUT",      help "Path to input file"]
        outputFile = strOption       . mconcat $ [long "output",     metavar "OUTPUT",     help "Path to output file"]
        directed   = flag False True . mconcat $ [long "directed",                         help "Whether the graph is directed"]
        seed       = option auto     . mconcat $ [long "seed",       metavar "SEED",       help "Random number seed"]
        iterations = option auto     . mconcat $ [long "iterations", metavar "ITERATIONS", help "Number of iterations to run per node"]

--------------------------------------------------------------------------------

toText :: Map Node Double -> Text
toText m = T.unlines [ T.unwords [T.pack node, T.pack (show spreadingPower)]
                     | (node, spreadingPower) <- M.assocs m
                     ]


go :: Options -> IO ()
go Options{..} = do
    mgraph <- parse (parseGraph directed) inputFile <$> T.readFile inputFile

    case mgraph of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right graph -> do
            let gen = mkStdGen seed
                lam = epidemicThreshold graph
                s   = evalState (spreadingPower iterations graph lam) gen
                t   = toText s
            T.writeFile outputFile t


main :: IO ()
main = execParser opts >>= go
    where
        opts = info (helper <*> options)
                    (mconcat [fullDesc, progDesc "spreading-power", header ""])
