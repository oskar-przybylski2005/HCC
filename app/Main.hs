module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec (errorBundlePretty)

import qualified Lexer
import qualified Parser
import qualified PrettyTree


main :: IO ()
main = do
    args <- getArgs
    let fileName = case args of
            (a:_) -> a
            _     -> error "No files specified"
    tokens <- Lexer.lexer fileName
    Lexer.printTokens tokens
    case Parser.parse fileName tokens of
        Right p -> PrettyTree.printTree p
        Left errBundle -> do
            hPutStrLn stderr "\n Error while parsing: "
            hPutStrLn stderr (errorBundlePretty errBundle)
            exitFailure
