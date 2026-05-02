module LexerSpec (spec) where

import Test.Hspec
import System.IO (readFile, writeFile)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath (takeExtension, replaceExtension, (</>))
import System.Environment (lookupEnv)
import Control.Monad (forM_)
import Text.Megaparsec.Pos (sourceName, sourceLine, sourceColumn, unPos)
import Lexer (lexer)
import Common

formatTokens :: [LocatedToken] -> String
formatTokens tokens = unlines (map formatToken tokens ++ ["=== End Of Tokens ==="])
  where
    formatToken (LToken tok pos txt) =
        let file = sourceName pos
            line = unPos (sourceLine pos)
            col  = unPos (sourceColumn pos)
            loc  = file ++ ":" ++ show line ++ ":" ++ show col
        in "[" ++ loc ++ "] " ++ show tok ++ " " ++ show txt

spec :: Spec
spec = describe "Lexer file tests" $ do
  (cFiles, shouldGenerate) <- runIO $ do
    let dataDir = "test/data"
    allFiles <- listDirectory dataDir
    let cfs = filter (\f -> takeExtension f == ".c") allFiles
    genEnv <- lookupEnv "GENERATE_TKS"
    return (cfs, genEnv == Just "1")

  let dataDir = "test/data"

  forM_ cFiles $ \cFile -> do
    let tokensFile = replaceExtension cFile ".tks"
    let cPath = dataDir </> cFile
    let tksPath = dataDir </> tokensFile

    it ("Testing: " ++ cFile) $ do
      actualTokens <- lexer cPath
      let actualOutput = formatTokens actualTokens
      
      exists <- doesFileExist tksPath
      if not exists || shouldGenerate
        then do
          putStrLn $ "  [GEN] Generating tks for: " ++ tksPath
          writeFile tksPath actualOutput
        else do
          expectedOutput <- readFile tksPath
          actualOutput `shouldBe` expectedOutput
