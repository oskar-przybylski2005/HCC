module Preprocessor where

import Common
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf)
import System.FilePath (takeDirectory, (</>))
import System.Directory (doesFileExist)
import Text.Megaparsec.Pos (initialPos, sourceName, SourcePos)

import Text.Megaparsec (runParserT, eof)
import Control.Monad.State.Strict (evalState)
import qualified Data.Set as Set
import Data.Bits (complement, (.&.), (.|.), xor, shiftL, shiftR)

import Parser.ExprParser (parseExpr)
import Parser.Common (Error)
import Parser.AST

type MacroEnv = Map.Map String [LocatedToken]

data LexerEnv = LexerEnv {
    envPos :: SourcePos,
    envMacros :: MacroEnv
}

data LexResult
    = LexDone LexerEnv
    | LexNeedInclude String SourcePos LexerEnv String

type LexerFn = LexerEnv -> String -> ([LocatedToken], LexResult)

-- takes source position and macro symbol, expands it and updates position
expandMacro :: MacroEnv -> SourcePos -> String -> ([LocatedToken], SourcePos)
expandMacro env pos macroName =
    case Map.lookup macroName env of
        Just macroTokens -> expandMacroList safeEnv pos macroTokens
        Nothing -> ([], pos)
    where safeEnv = Map.delete macroName env

expandMacroList :: MacroEnv -> SourcePos -> [LocatedToken] -> ([LocatedToken], SourcePos)
expandMacroList _ pos [] = ([], pos)
expandMacroList env pos (t:ts) =  case t of
            LToken TokenSymbol _ txt | Map.member txt env -> (expanded <> rest, finalPos)
                 where (expanded, posAfterMacro) = expandMacro env pos txt
                       (rest, finalPos) = expandMacroList env posAfterMacro ts
            _ -> (newT : rest, finalPos)
                where newT = t { lPos = pos }
                      nextPos = advStr pos (lText t)
                      (rest, finalPos) = expandMacroList env nextPos ts

parseDirective :: LexerFn -> LexerEnv -> String -> ([LocatedToken], LexResult)
parseDirective lexeFn env s =
    let (line, rest) = readTill "\n" s
        afterHash = dropWhile (`elem` " \t") (drop 1 line)
        (dirName, afterDir) = span isSymbolMid afterHash
    in case dirName of
        "define"  -> parseDefine lexeFn env line rest afterDir
        "include" -> parseInclude lexeFn env line rest afterDir
        "ifdef"   -> parseIfdef True lexeFn env line rest afterDir
        "ifndef"  -> parseIfdef False lexeFn env line rest afterDir
        "if"      -> parseIf lexeFn env line rest afterDir
        "elif"    -> parseElif lexeFn env line rest
        "else"    -> parseElse lexeFn env line rest
        "endif"   -> parseEndif lexeFn env line rest
        "undef"   -> parseUndef lexeFn env line rest afterDir
        "warning" -> parseIgnore lexeFn env line rest
        "error"   -> parseIgnore lexeFn env line rest
        "pragma"  -> parseIgnore lexeFn env line rest
        _         -> 
            let pos = envPos env
                emitedToken = LToken TokenInvalid pos line
                newEnv = env {envPos = advStr pos line}
                (restTokens, result) = lexeFn newEnv rest
            in (emitedToken : restTokens, result)

parseIgnore :: LexerFn -> LexerEnv -> String -> String -> ([LocatedToken], LexResult)
parseIgnore lexeFn env line rest =
    let newPos = advStr (envPos env) line
    in lexeFn (env {envPos = newPos}) rest

parseUndef :: LexerFn -> LexerEnv -> String -> String -> String -> ([LocatedToken], LexResult)
parseUndef lexeFn env line rest afterDir =
    let symbolName = takeWhile isSymbolMid (dropWhile (`elem` " \t") afterDir)
        newMacros = Map.delete symbolName (envMacros env)
        newPos = advStr (envPos env) line
    in lexeFn (env {envPos = newPos, envMacros = newMacros}) rest

skipBlock :: Bool -> Int -> String -> (String, String)
skipBlock _ _ [] = ("", [])
skipBlock stopAtElse depth s =
    let (line, rest) = readTill "\n" s
        trimmed = dropWhile (`elem` " \t") line
    in if not (null trimmed) && head trimmed == '#'
       then let afterHash = dropWhile (`elem` " \t") (tail trimmed)
                (dirName, _) = span isSymbolMid afterHash
            in case dirName of
                "ifdef"  -> let (skippedRest, finalRest) = skipBlock stopAtElse (depth + 1) rest
                            in (line ++ skippedRest, finalRest)
                "ifndef" -> let (skippedRest, finalRest) = skipBlock stopAtElse (depth + 1) rest
                            in (line ++ skippedRest, finalRest)
                "if"     -> let (skippedRest, finalRest) = skipBlock stopAtElse (depth + 1) rest
                            in (line ++ skippedRest, finalRest)
                "endif"  -> if depth == 1
                            then ("", s)
                            else let (skippedRest, finalRest) = skipBlock stopAtElse (depth - 1) rest
                                 in (line ++ skippedRest, finalRest)
                "else"   -> if stopAtElse && depth == 1
                            then ("", s)
                            else let (skippedRest, finalRest) = skipBlock stopAtElse depth rest
                                 in (line ++ skippedRest, finalRest)
                "elif"   -> if stopAtElse && depth == 1
                            then ("", s)
                            else let (skippedRest, finalRest) = skipBlock stopAtElse depth rest
                                 in (line ++ skippedRest, finalRest)
                _        -> let (skippedRest, finalRest) = skipBlock stopAtElse depth rest
                            in (line ++ skippedRest, finalRest)
       else let (skippedRest, finalRest) = skipBlock stopAtElse depth rest
            in (line ++ skippedRest, finalRest)

replaceDefined :: MacroEnv -> String -> String
replaceDefined env = go False
  where
    go _ [] = []
    go isPrevSym s
        | not isPrevSym && "defined" `isPrefixOf` s =
            let rest = drop 7 s
                isEndOfWord = null rest || not (isSymbolMid (head rest))
            in if isEndOfWord
               then
                   let afterWord = dropWhile (`elem` " \t") rest
                   in if not (null afterWord) && head afterWord == '('
                      then let inside = dropWhile (`elem` " \t") (tail afterWord)
                               sym = takeWhile isSymbolMid inside
                               afterSym = dropWhile (`elem` " \t") (drop (length sym) inside)
                           in if not (null afterSym) && head afterSym == ')'
                              then (if Map.member sym env then " 1 " else " 0 ") ++ go False (tail afterSym)
                              else "defined" ++ go True rest
                      else let sym = takeWhile isSymbolMid afterWord
                               afterSym = drop (length sym) afterWord
                           in if not (null sym)
                              then (if Map.member sym env then " 1 " else " 0 ") ++ go False afterSym
                              else "defined" ++ go True rest
               else head s : go (isSymbolMid (head s)) (tail s)
        | otherwise = head s : go (isSymbolMid (head s)) (tail s)

evalAST :: Expression -> Integer
evalAST (IntegerExp i) = i
evalAST (CharExp c) = toInteger (fromEnum c)
evalAST (StringExp _) = 0
evalAST (UnaryExp op a) =
    let va = evalAST a
    in case op of
        Not -> if va == 0 then 1 else 0
        MinusSign -> -va
        FirstCompl -> complement va
        _ -> va
evalAST (BinExp op a b) =
    let va = evalAST a
        vb = evalAST b
    in case op of
        Addition -> va + vb
        Substraction -> va - vb
        Multiplication -> va * vb
        Division -> if vb == 0 then 0 else va `div` vb
        Modulo -> if vb == 0 then 0 else va `mod` vb
        Equal -> if va == vb then 1 else 0
        NotEqual -> if va /= vb then 1 else 0
        Less -> if va < vb then 1 else 0
        Leq -> if va <= vb then 1 else 0
        Greater -> if va > vb then 1 else 0
        Geq -> if va >= vb then 1 else 0
        LogicAnd -> if va /= 0 && vb /= 0 then 1 else 0
        LogicOr -> if va /= 0 || vb /= 0 then 1 else 0
        BitAnd -> va .&. vb
        BitOr -> va .|. vb
        BitXor -> va `xor` vb
        BitShiftL -> va `shiftL` fromInteger vb
        BitShiftR -> va `shiftR` fromInteger vb
        _ -> 0
evalAST _ = 0

parseExprTokens :: [LocatedToken] -> Either Error Expression
parseExprTokens toks = evalState (runParserT (parseExpr <* eof) "" toks) Set.empty

skipToNextBranch :: LexerFn -> LexerEnv -> String -> ([LocatedToken], LexResult)
skipToNextBranch lexeFn env s =
    let (skipped, finalRest) = skipBlock True 1 s
        newPos = advStr (envPos env) skipped
        newEnv = env { envPos = newPos }
    in if null finalRest then lexeFn newEnv finalRest else
       let (line, restAfterDir) = readTill "\n" finalRest
           trimmed = dropWhile (`elem` " \t") line
           afterHash = dropWhile (`elem` " \t") (tail trimmed)
           (dirName, afterDir) = span isSymbolMid afterHash
       in case dirName of
           "elif" ->
               let exprString = replaceDefined (envMacros newEnv) (dropWhile (`elem` " \t") afterDir)
                   (toks, _) = lexeFn (newEnv { envPos = initialPos "" }) exprString
                   cleanToks = filter ((/= TokenEnd) . lToken) toks
                   parsed = parseExprTokens cleanToks
                   condition = case parsed of
                       Right ast -> evalAST ast /= 0
                       Left _    -> False
                   newPosElif = advStr newPos line
               in if condition
                  then lexeFn (newEnv { envPos = newPosElif }) restAfterDir
                  else skipToNextBranch lexeFn (newEnv { envPos = newPosElif }) restAfterDir
           "else" ->
               let newPosElse = advStr newPos line
               in lexeFn (newEnv { envPos = newPosElse }) restAfterDir
           "endif" ->
               let newPosEndif = advStr newPos line
               in lexeFn (newEnv { envPos = newPosEndif }) restAfterDir
           _ ->
               lexeFn newEnv finalRest

parseIf :: LexerFn -> LexerEnv -> String -> String -> String -> ([LocatedToken], LexResult)
parseIf lexeFn env line rest afterDir =
    let exprString = replaceDefined (envMacros env) (dropWhile (`elem` " \t") afterDir)
        (toks, _) = lexeFn (env { envPos = initialPos "" }) exprString
        cleanToks = filter ((/= TokenEnd) . lToken) toks
        parsed = parseExprTokens cleanToks
        condition = case parsed of
            Right ast -> evalAST ast /= 0
            Left _    -> False
    in if condition
       then let newPos = advStr (envPos env) line
            in lexeFn (env {envPos = newPos}) rest
       else skipToNextBranch lexeFn (env {envPos = advStr (envPos env) line}) rest

parseIfdef :: Bool -> LexerFn -> LexerEnv -> String -> String -> String -> ([LocatedToken], LexResult)
parseIfdef isIfdef lexeFn env line rest afterDir =
    let symbolName = takeWhile isSymbolMid (dropWhile (`elem` " \t") afterDir)
        isDefined = Map.member symbolName (envMacros env)
        condition = if isIfdef then isDefined else not isDefined
    in if condition
       then let newPos = advStr (envPos env) line
            in lexeFn (env {envPos = newPos}) rest
       else skipToNextBranch lexeFn (env {envPos = advStr (envPos env) line}) rest

parseElif :: LexerFn -> LexerEnv -> String -> String -> ([LocatedToken], LexResult)
parseElif lexeFn env line rest =
    let (skipped, finalRest) = skipBlock False 1 rest
        newPos = advStr (envPos env) (line ++ skipped)
    in lexeFn (env {envPos = newPos}) finalRest

parseElse :: LexerFn -> LexerEnv -> String -> String -> ([LocatedToken], LexResult)
parseElse lexeFn env line rest =
    let (skipped, finalRest) = skipBlock False 1 rest
        newPos = advStr (envPos env) (line ++ skipped)
    in lexeFn (env {envPos = newPos}) finalRest

parseEndif :: LexerFn -> LexerEnv -> String -> String -> ([LocatedToken], LexResult)
parseEndif lexeFn env line rest =
    let newPos = advStr (envPos env) line
    in lexeFn (env {envPos = newPos}) rest

parseDefine :: LexerFn -> LexerEnv -> String -> String -> String -> ([LocatedToken], LexResult)
parseDefine lexeFn env line rest afterDir =
    let pos = envPos env
        (macroName, macroBodyStr) = span isSymbolMid (dropWhile (`elem` " \t") afterDir)
        macroBodyStrTrimmed = dropWhile (`elem` " \t") macroBodyStr
        cleanBody = filter (/= '\n') macroBodyStrTrimmed
        envForInnerParsing = LexerEnv pos Map.empty
        (macroTokens, _) = lexeFn envForInnerParsing cleanBody
        newMacro = filter ((/= TokenEnd) . lToken) macroTokens
        newMacroMap = Map.insert macroName newMacro (envMacros env)
        newPos = advStr pos line
        newEnv = env {envPos = newPos, envMacros = newMacroMap}
    in lexeFn newEnv rest

parseInclude :: LexerFn -> LexerEnv -> String -> String -> String -> ([LocatedToken], LexResult)
parseInclude _ env line rest afterDir =
    let afterInclude = dropWhile (`elem` " \t") afterDir
        pos = envPos env
        newPos = advStr pos line
        newEnv = env {envPos = newPos}
    in ([], LexNeedInclude afterInclude pos newEnv rest)

-- IO driver that recursively resolves includes
runLexerIO :: LexerFn -> LexerEnv -> String -> IO ([LocatedToken], LexerEnv)
runLexerIO lexeFn env input = do
    let (tokens, result) = lexeFn env input
    case result of
        LexDone finalEnv -> pure (tokens, finalEnv)
        LexNeedInclude fileStr pos afterIncEnv restStr -> do
            let isLocal = case fileStr of
                            ('"':_) -> True
                            _       -> False
            let fileName = takeWhile (\c -> c /= '"' && c /= '>') (drop 1 fileStr)
            let currentDir = takeDirectory (sourceName pos)
            let stdPaths = [ "/usr/include"
                           , "/usr/local/include"
                           , "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"
                           ]
            let pathsToTry = if isLocal
                             then [currentDir </> fileName, fileName]
                             else map (</> fileName) stdPaths ++ [fileName, currentDir </> fileName]

            foundPath <- findFile pathsToTry
            (includeTokens, envAfterInclude) <- case foundPath of
                Just p -> do
                    content <- readFile p
                    let includePos = initialPos p
                    let includeEnv = LexerEnv includePos (envMacros afterIncEnv)
                    runLexerIO lexeFn includeEnv (spliceLines content)
                Nothing -> do
                    putStrLn $ "Warning: Could not find included file: " ++ fileName
                    let errLine = takeWhile (/= '\n') fileStr
                    pure ([LToken TokenInvalid pos errLine], afterIncEnv)

            let cleanedIncludeTokens = filter ((/= TokenEnd) . lToken) includeTokens

            let resumedEnv = afterIncEnv { envMacros = envMacros envAfterInclude }
            (restTokens, finalEnv) <- runLexerIO lexeFn resumedEnv restStr

            pure (tokens ++ cleanedIncludeTokens ++ restTokens, finalEnv)

findFile :: [FilePath] -> IO (Maybe FilePath)
findFile [] = pure Nothing
findFile (p:ps) = do
    exists <- doesFileExist p
    if exists then pure (Just p) else findFile ps
