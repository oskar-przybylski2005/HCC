module Lexer(
    lexer,
    -- Wystawione dla testów jednostkowych:
    isHexStart,
    extractHex,
    readLit,
    isValidIntSuffix,
    consumeIntSuffix,
    consumeExponent,
    consumeFloatSuffix,
    lexe,
    LexResult(..)
) where

import qualified Data.Map.Strict as Map
import GHC.List (uncons)
import Text.Megaparsec hiding (Token)

import Preprocessor
import Common

-- PUBLIC API
-- wraper for reading from file
lexer :: String -> IO [LocatedToken]
lexer fileName = do
    fileContent <- readFile fileName
    let startPos = initialPos fileName
    let lexerEnv = LexerEnv startPos Map.empty
    (tokens, _) <- runLexerIO lexe lexerEnv (spliceLines fileContent)
    pure $ postProcess tokens

isKeyWord :: String -> Bool
isKeyWord str = str `elem` [
    "static", "extern", "auto", "register",
    "typedef", "struct", "union", "enum", "sizeof",
    "short","int","long","float","const","default",
    "double","void","if","else","char","static",
    "switch","case","break","continue","goto",
    "while","for","return","struct","typedef"]

isHexStart :: String -> Bool
isHexStart []     = False
isHexStart (x:xs) = x == '0' && case uncons xs of
                                    Just (h, _ ) | h == 'x' || h == 'X' -> True
                                    _ -> False

extractHex :: String -> (String, String)
extractHex str = span isHex after0x
    where
        after0x = drop 2 str

readLit :: Char -> String -> (String, String)
readLit endChar = go []
    where
        go acc [] = (reverse acc, [])
        go acc ['\\'] = (reverse ('\\':acc), [])
        go acc ('\\':c:cs) = go (c:'\\':acc) cs
        go acc (c:cs)
            | c == endChar = (reverse (c:acc), cs)
            | otherwise = go (c:acc) cs

-- Helper to emit a token and advance recursion seamlessly
emit :: Token -> String -> LexerEnv -> String  -> ([LocatedToken], LexResult)
emit token text env rest = let
    pos = envPos env
    emitedToken = LToken token pos text
    newEnv = env {envPos = advStr pos text}
    (restTokens, result) = lexe newEnv rest
    in (emitedToken : restTokens, result)

isValidIntSuffix :: String -> Bool
isValidIntSuffix suf = map toLowerC suf `elem` validSuffixes
    where
    toLowerC c | c `elem` ['A'..'Z'] = toEnum (fromEnum c + 32)
               | otherwise = c
    validSuffixes =
        [ ""                       -- no suffix
        , "u"                      -- unsigned
        , "l"                      -- long
        , "ll"                     -- long long
        , "ul"                     -- long unsigned
        , "lu"                     -- long unsigned
        , "ull"                    -- long long unsigned
        , "llu"                    -- long long unsigned
        ]

consumeIntSuffix :: String -> (String, String)
consumeIntSuffix xs =
    let (suf, rest) = span isSuffixChar xs
    in if isValidIntSuffix suf
        then (suf, rest)
        else ("", xs)
  where
    isSuffixChar c = c `elem` "uUlL"

consumeExponent :: String -> (String, String)
consumeExponent [] = ("", [])
consumeExponent s@(e:rest)
    | e == 'e' || e == 'E' =
        let (sign, afterSign) = case rest of
                ('+':ys) -> ("+", ys)
                ('-':ys) -> ("-", ys)
                _        -> ("", rest)
            (digits, afterDigits) = span isNum afterSign
        in if null digits
           then ("", s)
           else (e : sign ++ digits, afterDigits)
    | otherwise = ("", s)

consumeFloatSuffix :: String -> String
consumeFloatSuffix ('f':zs) = zs
consumeFloatSuffix ('F':zs) = zs
consumeFloatSuffix ('l':zs) = zs
consumeFloatSuffix ('L':zs) = zs
consumeFloatSuffix zs = zs

lexeSymbol :: LexerEnv -> String -> ([LocatedToken], LexResult)
lexeSymbol _ [] = ([], LexDone (LexerEnv (initialPos "") Map.empty)) -- fallback
lexeSymbol env (x:xs)= let
        (sym, rest) = span isSymbolMid xs
        fullSymbol = x : sym
        macroMap = envMacros env
        pos = envPos env
        (expanded, nextPos) = expandMacro macroMap pos fullSymbol
        updatedEnv = env {envPos = nextPos}
        tokenKind | isKeyWord fullSymbol = TokenKeyWord
                  | otherwise = TokenSymbol

        (restTokens, result) = lexe updatedEnv rest
        expandMacroAndContinue = (expanded ++ restTokens, result)

        in if Map.member fullSymbol macroMap then expandMacroAndContinue
        else emit tokenKind fullSymbol env rest


lexe :: LexerFn
lexe env [] = ([LToken TokenEnd (envPos env) ""], LexDone env)
lexe env s@(x:xs)
    -- preprocessor directives
    | x == '#'  = parseDirective lexe env s
    -- chars literals
    | x == '\'' =
        let (readed, rest) = readLit '\'' xs
         in emit TokenCharLit (x:readed) env rest
    -- string literals
    | x == '"' =
        let (readed, rest) = readLit '"' xs
        in emit TokenStrLit (x:readed) env rest
    -- hex numbers (prefix is 0x or 0X)
    | isHexStart s =
        let (hex, rest) = extractHex s
            consumed = take (2 + length hex) s
        in emit TokenHexLit consumed env rest
    | x == '/' = case xs of
        -- inline comments
        ('/':_) -> let
                (readed, rest) = readTill "\n" xs -- consume comment content
                newPos = advStr (envPos env) (x:readed)
                in lexe (env { envPos = newPos }) rest

        -- multiline comments
        ('*':_) -> let
                (_, rest) = readTill "*/" xs
                newPos = advPos (envPos env) '/'
                in lexe (env { envPos = newPos }) rest
        -- /= operator
        ('=':ys) -> emit TokenDivEq  "/=" env ys
        _        -> emit TokenSlashR "/"  env xs
    -- operators
    | x == '^' = case xs of
                    ('=':ys)     -> emit TokenXorEq  "^="  env ys
                    _            -> emit TokenBitXor "^"   env xs
    | x == '%' = case xs of
                    ('=':ys)     -> emit TokenModEq  "%="  env ys
                    _            -> emit TokenMod    "%"   env xs
    | x == '>' = case xs of
                    ('>':'=':ys) -> emit TokenRSEq   ">>=" env ys
                    ('>':ys)     -> emit TokenRShift ">>"  env ys
                    ('=':ys)     -> emit TokenGeq    ">="  env ys
                    _            -> emit TokenGreater ">"   env xs
    | x == '<' = case xs of
                    ('<':'=':ys) -> emit TokenLSEq   "<<=" env ys
                    ('<':ys)     -> emit TokenLShift "<<"  env ys
                    ('=':ys)     -> emit TokenLeq    "<="  env ys
                    _            -> emit TokenLess   "<"   env xs
    | x == '-' = case xs of
                    ('-':ys)     -> emit TokenDminus "--"  env ys
                    ('>':ys)     -> emit TokenArrow  "->"  env ys
                    ('=':ys)     -> emit TokenMeq    "-="  env ys
                    _            -> emit TokenMinus  "-"   env xs
    | x == '+' = case xs of
                    ('+':ys)     -> emit TokenDplus  "++"  env ys
                    ('=':ys)     -> emit TokenPeq    "+="  env ys
                    _            -> emit TokenPlus   "+"   env xs
    | x == '&' = case xs of
                    ('&':ys)     -> emit TokenAnd    "&&"  env ys
                    ('=':ys)     -> emit TokenAndEq  "&="  env ys
                    _            -> emit TokenBitAnd "&"   env xs
    | x == '|' = case xs of
                    ('|':ys)     -> emit TokenOr     "||"  env ys
                    ('=':ys)     -> emit TokenOrEq   "|="  env ys
                    _            -> emit TokenBitOr  "|"   env xs
    | x == '!' = case xs of
                    ('=':ys)     -> emit TokenNeq    "!="  env ys
                    _            -> emit TokenNot    "!"   env xs
    | x == '*' = case xs of
                    ('=':ys)     -> emit TokenSEq    "*="  env ys
                    _            -> emit TokenStar   "*"   env xs
    | x == '=' = case xs of
                    ('=':ys)     -> emit TokenDeq    "=="  env ys
                    _            -> emit TokenEq     "="   env xs

    -- floats starting with .
    | x == '.' && maybe False (isNum . fst) (uncons xs) = let
        (_ , rest) = span isNum xs
        (_, afterE) = consumeExponent rest
        r = consumeFloatSuffix afterE
        ( consumedRaw , _ ) = splitAt (length s - length r) s
        in emit TokenFloatLit consumedRaw env r
    -- numbers
    | isNum x = let
        (_ , rest) = span isNum xs
        (kind, toParse) = case rest of
            -- Floats
            ('.': ys ) -> (TokenFloatLit, r)
               where (_ , restF) = span isNum ys
                     (_, afterE) = consumeExponent restF
                     r = consumeFloatSuffix afterE
            -- Scientific notation
            (e:ys) | e == 'e' || e == 'E' ->
                let (ePart, afterE) = consumeExponent (e:ys)
                in if null ePart
                   then (TokenIntLit, snd (consumeIntSuffix rest))
                   else (TokenSciLit, consumeFloatSuffix afterE)
            -- Normal ints
            _ -> (TokenIntLit, snd (consumeIntSuffix rest))

        -- Crucial: reconstruct original text to ensure correct position advancement
        ( consumedRaw, _ ) = splitAt (length s - length toParse) s
        in emit kind consumedRaw env toParse

    -- symbols
    | isSymbolStart x = lexeSymbol env s

    -- does not emit newlines
    | x `elem` "\n" = let
        newPos = advPos (envPos env) x
        newEnv = env {envPos = newPos}
        in lexe newEnv xs

    -- other one char tokens and Invalid Token
    | otherwise =
        let tk = tokenKind x
            txt = [x]
            pos = envPos env
        in if tk == TokenInvalid
            then ([LToken TokenInvalid pos txt], LexDone env)
            else emit tk txt env xs
     where
       tokenKind c
           | c == ';' = TokenSemi     | c == '(' = TokenParL    | c == ')'  = TokenParR
           | c == '{' = TokenBraL     | c == '}' = TokenBraR    | c == '['  = TokenSqrL
           | c == ']' = TokenSqrR     | c == ',' = TokenComma   | c == '.'  = TokenDot
           | c == ':' = TokenDblDot   | c == '?' = TokenTernary | c == '~'  = TokenTilda
           | c == ' ' = TokenSpace    | c == '\t' = TokenTab    | otherwise = TokenInvalid
