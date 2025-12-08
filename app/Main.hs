module Main (main) where

import System.Environment (getArgs)

import qualified Lexer
import qualified Parser
import qualified PrettyTree

main :: IO ()
main = do
    args <- getArgs
    tokens <- concat <$> mapM Lexer.lexer args
    case Parser.parse tokens of
        Just (prog, []) -> PrettyTree.printProgram prog
        _               -> putStrLn "Parse error"
