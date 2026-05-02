module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hPutStr,stderr)
import Text.Megaparsec (errorBundlePretty)

import Common
import Parser.AST

import qualified Lexer
import qualified Parser

main :: IO ()
main = do
    args <- getArgs
    let fileName = case args of
            (a:_) -> a
            _     -> error "No files specified"
    tokens <- Lexer.lexer fileName
    printTokens tokens
    case Parser.parse fileName tokens of
       Right p -> prettyPrint p
       Left errBundle -> do
           hPutStr   stderr "\n Error occured while parsing file: "
           hPutStrLn stderr fileName
           hPutStrLn stderr (errorBundlePretty errBundle)
           exitFailure
