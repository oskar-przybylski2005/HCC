module Lexer where

import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)

import GHC.List (unsnoc,uncons)

data Token
    = TokenEnd      -- EOF
    | TokenDirective String -- #include / #define
    | TokenInclArg   String -- <whatever>
    | TokenSymbol    String -- ^[A-Za-z_][A-Za-z0-9_]*$
    | TokenKeyWord   String -- int,chat,float,double ...
    | TokenIntLit    String -- 0,1,2,...
    | TokenSciLit    String -- 10e100
    | TokenHexLit    String -- 0x001, 0xFF, ...
    | TokenFloatLit  String -- 0.11, 32.14, ...
    | TokenSemi     -- semicolon
    | TokenParR     -- )
    | TokenParL     -- (
    | TokenBraR     -- }
    | TokenBraL     -- {
    | TokenSqrR     -- ]
    | TokenSqrL     -- [
    | TokenComma    -- ,
    | TokenDot      -- .
    | TokenNewLine  -- \n
    | TokenStrLit   String -- "whatever"
    | TokenCharLit  Char   -- 'x'
    | TokenComment  -- // comment or /* comment */
    | TokenEq       -- =
    | TokenDeq      -- ==
    | TokenNot      -- !
    | TokenNeq      -- !=
    | TokenOr       -- ||
    | TokenOrEq     -- |=
    | TokenBitOr    -- |
    | TokenBitXor   -- ^
    | TokenXorEq    -- ^=
    | TokenAnd      -- &&
    | TokenAndEq    -- &=
    | TokenBitAnd   -- &
    | TokenStar     -- *
    | TokenSEq      -- *=
    | TokenPlus     -- +
    | TokenPeq      -- +=
    | TokenDplus    -- ++
    | TokenMinus    -- -
    | TokenMeq      -- -=
    | TokenDminus   -- --
    | TokenArrow    -- ->
    | TokenLeq      -- <=
    | TokenLShift   -- <<
    | TokenLSEq     -- <<=
    | TokenLess     -- <
    | TokenGeq      -- >=
    | TokenRShift   -- >>
    | TokenRSEq     -- >>=
    | TokenGreater  -- >
    | TokenDivide   -- /
    | TokenDivEq    -- /=
    | TokenMod      -- %
    | TokenModEq    -- %=
    | TokenDblDot   -- :  (goto labels, and ternary operator)
    | TokenTernary  -- ?
    | TokenTilda    -- ~

    | TokenInvalid  -- invalid token -> error
    deriving (Show, Eq)

printTokens :: [Token] -> IO()
printTokens [] = return ()
printTokens (x:xs) = do
    putStrLn $ show x
    printTokens xs

unescapeString :: String -> String
unescapeString = go
  where
    go [] = []
    go ('\\':x:xs) = case x of
        'n'  -> '\n' : go xs
        't'  -> '\t' : go xs
        'r'  -> '\r' : go xs
        '0'  -> '\0' : go xs
        '\\' -> '\\' : go xs
        '"'  -> '"'  : go xs
        '\'' -> '\'' : go xs

        -- hex escape \xFF
        'x'  ->
            let (hs, rest) = splitAt 2 xs
            in if all isHex hs
                then toEnum (read ("0x" ++ hs)) : go rest
                else x : go xs

        -- octal escape \123
        d | d >= '0' && d <= '7' ->
            let (os, rest) = span (`elem` ['0'..'7']) (x:xs)
                val = foldl (\acc c -> acc * 8 + digitToInt c) 0 os
            in toEnum val : go rest

        _ -> x : go xs  -- unknown escape → literally

    go (x:xs) = x : go xs

isNum :: Char -> Bool
isNum c = c `elem` ['0'..'9']

isHex :: Char -> Bool
isHex c = isNum c || c `elem` ['A'..'F'] || c `elem` ['a'..'f']

isAlpnum :: Char -> Bool
isAlpnum c
    | isAlpha c = True
    | isNum   c = True
    | otherwise = False

isAlpha :: Char -> Bool
isAlpha c
    | c `elem` ['A'..'Z'] = True
    | c `elem` ['a'..'z'] = True
    | otherwise           = False

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c == '_'

isSymbolMid :: Char -> Bool
isSymbolMid c = isAlpnum c || c == '_'

isSymbol :: String -> Bool
isSymbol []     = False
isSymbol (x:xs) =
        isSymbolStart x && all isSymbolMid xs

isWhiteSpace :: Char -> Bool
isWhiteSpace x = x `elem` [' ','\r','\t']

isKeyWord :: String -> Bool
isKeyWord str = str `elem` [
    "typedef", "struct", "union", "enum", "sizeof",
    "short","int","long","float","const","default",
    "double","void","if","else","char","static",
    "switch","case","break","continue","goto",
    "while","for","return","struct","typedef"]

isHexStart :: String -> Bool
isHexStart []     = False
isHexStart (x:xs) = x == '0' && case uncons xs of
                                    Just (h, _ ) | h == 'x' -> True
                                    _ -> False

extractHex :: String -> (String, String)
extractHex str = span isHex after0x
    where
        after0x = fromMaybe "" (stripPrefix "0x" str)

-- takes a char and returns function that goes till it finds that char (that is not escaped)
-- then returns whole symbol and rest
readLit :: Char -> String -> (String, String)
readLit end = go []
  where
    go acc [] = (reverse acc, [])
    go acc (x:xs)
      | x == '\\' = case xs of
          []     -> go ('\\':acc) []
          (y:ys) -> go (y:'\\':acc) ys
      | x == end = (reverse (end:acc), xs)
      | otherwise = go (x:acc) xs

-- takes a str and returns function that goes till it finds that str
-- then returns whole symbol and rest
readLitStr :: String -> String -> (String, String)
readLitStr end = go
  where
    n = length end
    go [] = ([], [])
    go s@(x:xs)
      | end `isPrefixOf` s = (end, drop n s)
      | otherwise =
          let (lit, rest) = go xs
          in (x : lit, rest)


isValidIntSuffix :: String -> Bool
isValidIntSuffix suf = suf `elem` validSuffixes
    where
    validSuffixes =
        [ ""                       -- no suffix
        , "u","U"                  -- unsigned
        , "l","L"                  -- long
        , "ll","LL"                -- long long
        , "ul","uL","Ul","UL"      -- long unsigned
        , "lu","lU","Lu","LU"      -- long unsigned
        , "ull","uLL","Ull","ULL"  -- long long unsigned
        , "llu","llU","LLu","LLU"  -- long long unsigned
        ]

consumeIntSuffix :: String -> (String, String)
consumeIntSuffix xs =
    let (suf, rest) = span isSuffixChar xs
    in if isValidIntSuffix suf
        then (suf, rest)
        else ("", xs)
  where
    isSuffixChar c = c `elem` "uUlL"

lexe :: String -> [Token]
lexe [] = [TokenEnd]
lexe s@(x:xs)
    | isWhiteSpace x = lexe xs
    | x == '/' = case xs of
        ('/':_) -> let
                ( _ , rest) = readLit '\n' xs
                 in TokenComment : lexe rest
        ('=':ys) -> TokenDivEq   : lexe ys
        ('*':_) -> let
                ( _ , rest) = readLitStr "*/" xs
                in TokenComment    : lexe rest
        _       -> TokenDivide     : lexe xs

    | x == '#' = let
        (lit, rest) = readLit '\n' xs
        fullDirective = x : lit
        in TokenDirective (init fullDirective)
        : lexe rest

    | x == '\'' =
        let (lit, rest) = readLit '\'' xs
            raw = x : lit
            inner = case unsnoc raw of
                Just (middle, _ ) -> drop 1 middle
                Nothing           -> error "unexpected empty literal"
            unescaped = unescapeString inner
            char = case uncons unescaped of
                Just (c, []) -> c
                _ -> error ("char literal is longer than one char: "++ show raw)
        in TokenCharLit char : lexe rest

    | x == '"' =
        let (lit, rest) = readLit '"' xs
            raw = x : lit
            inner = case unsnoc raw of
                Just (middle, _ ) -> drop 1 middle
                Nothing           -> error "unexpected empty literal"
            unescaped = unescapeString inner
        in TokenStrLit unescaped : lexe rest

    | x ==  '^' = case xs of
        ('=':ys) -> TokenXorEq  : lexe ys
        _ ->        TokenBitXor : lexe xs

    | x ==  '%' = case xs of
        ('=':ys) -> TokenModEq  : lexe ys
        _ ->        TokenMod    : lexe xs

    | x ==  '>' = case xs of
        ('>':'=':ys) -> TokenRSEq: lexe ys
        ('>':ys) -> TokenRShift  : lexe ys
        ('=':ys) -> TokenGeq     : lexe ys
        _ -> TokenGreater        : lexe xs

    | x ==  '<' = case xs of
        ('<':'=':ys) -> TokenLSEq : lexe ys
        ('<':ys) -> TokenLShift   : lexe ys
        ('=':ys) -> TokenLeq      : lexe ys
        _ -> TokenLess            : lexe xs

    | x ==  '-' = case xs of
        ('-':ys) -> TokenDminus : lexe ys
        ('>':ys) -> TokenArrow  : lexe ys
        ('=':ys) -> TokenMeq    : lexe ys
        _ ->        TokenMinus  : lexe xs

    | x ==  '+' = case xs of
        ('+':ys) -> TokenDplus  : lexe ys
        ('=':ys) -> TokenPeq    : lexe ys
        _ -> TokenPlus          : lexe xs

    | x ==  '&' = case xs of
        ('&':ys) -> TokenAnd    : lexe ys
        ('=':ys) -> TokenAndEq  : lexe ys
        _ -> TokenBitAnd        : lexe xs

    | x ==  '|' = case xs of
        ('|':ys) -> TokenOr     : lexe ys
        ('=':ys) -> TokenOrEq   : lexe ys
        _ -> TokenBitOr         : lexe xs

    | x ==  '!' = case xs of
        ('=':ys) -> TokenNeq    : lexe ys
        _  -> TokenNot          : lexe xs

    | x ==  '*' = case xs of
        ('=':ys) -> TokenSEq    : lexe ys
        _ -> TokenStar          : lexe xs

    | x ==  '=' = case xs of
        ('=':ys) -> TokenDeq    : lexe ys
        _ -> TokenEq            : lexe xs

    | isHexStart s = let
        (hex, rest) = extractHex s
        full = "0x" ++ hex
        in TokenHexLit full : lexe rest

    | x == '.' = let
        (float, rest) = span isNum xs
        baseFloat = "0." ++ float
        r = case rest of
            ('f':xs) -> xs   -- .14f
            ('F':xs) -> xs   -- .14F
            _        -> rest -- .14
        in TokenFloatLit baseFloat : lexe r

    | isNum x = let
        (prefix, rest) = span isNum xs
        fullPrefix = x : prefix
        (kind, val, toParse) = case rest of

            -- floats starting with int
            ('.': ys ) -> (TokenFloatLit, baseFloat, r)
             where
                (fracPart, restFloat) = span isNum ys
                baseFloat = fullPrefix ++ "." ++ fracPart ++ "0"
                -- ++ "0" is for 3.
                r = case restFloat of
                    ('f':xs) ->  xs -- 3.14f
                    ('F':xs) ->  xs -- 3.14F
                    _        ->  restFloat -- 3.14

            -- scientific notation with base "e" and exponent + optional f/F at the end
            ('e': y: ys ) | (isNum y || y=='-') -> (TokenSciLit, baseSci, r) where
                (exponent, restSci) = span isNum ys
                baseSci = fullPrefix ++ "e" ++ [y] ++ exponent
                r = case restSci of
                    ('f':xs) -> xs -- 3e14f
                    ('F':xs) -> xs -- 3e14F
                    _        -> restSci -- 3e14

            -- normal ints
            -- unsigned
            _ -> (TokenIntLit, fullPrefix ++ suf, rest')
                where (suf, rest') = consumeIntSuffix rest

        in kind val : lexe toParse

    | isSymbolStart x = let
        (sym, rest) = span isSymbolMid xs
        full = x : sym
        tokenKind | isKeyWord full = TokenKeyWord
                  | otherwise      = TokenSymbol
              in tokenKind full : lexe rest

    | otherwise =
        let tk = tokenKind x
        in if tk == TokenInvalid
            then [TokenInvalid]
            else tk : lexe xs
     where
        tokenKind c
            | c == ';'  = TokenSemi
            | c == '('  = TokenParL
            | c == ')'  = TokenParR
            | c == '{'  = TokenBraL
            | c == '}'  = TokenBraR
            | c == '['  = TokenSqrL
            | c == ']'  = TokenSqrR
            | c == ','  = TokenComma
            | c == '.'  = TokenDot
            | c == ':'  = TokenDblDot
            | c == '?'  = TokenTernary
            | c == '~'  = TokenTilda
            | c == '\n' = TokenNewLine
            | otherwise = TokenInvalid

lexer :: String -> IO([Token])
lexer fileName = do
                fileContent <- readFile fileName
                return $ lexe fileContent

-- main :: IO()
-- main = do
--    args <- getArgs
--    listOfTokenLists <- mapM lexer args
--    let tokens = concat listOfTokenLists
--    printTokens tokens
    -- printTokens $ filter (\x -> kind x == TokenComment) tokens


