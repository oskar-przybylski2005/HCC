module Lexer where

import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)

import GHC.List (uncons)

import Text.Megaparsec.Pos (
        SourcePos, unPos, sourceLine,
        initialPos, sourceName,
        mkPos, sourceColumn)

data LocatedToken = LToken {
    lToken :: Token,    -- Token
    lPos   :: SourcePos,-- Where the Token is starting (file, line, column)
    lText  :: String    -- Original Token Text eg. ("  int   ")
} deriving (Show, Eq)

printTokens :: [LocatedToken] -> IO ()
printTokens [] = putStrLn "=== End Of Tokens ==="
printTokens (LToken tok pos txt : xs) = do
    let file = sourceName pos
        line = unPos (sourceLine pos)
        col  = unPos (sourceColumn pos)
        loc  = file ++ ":" ++ show line ++ ":" ++ show col

    -- Format: [plik:linia:kolumna] Token "OryginalnyTekst"
    putStrLn $ "[" ++ loc ++ "] " ++ show tok ++ " " ++ show txt
    printTokens xs

-- advances position by one char
advPos :: SourcePos -> Char -> SourcePos
-- line, column -> line + 1, 1
advPos pos '\n' = l `setLine` pos
                    where l = getLine pos + 1
                          setLine l p = p {sourceLine   = mkPos l,
                                           sourceColumn = mkPos 1 }
                          getLine     = unPos . sourceLine

-- line, column -> line, column + x
advPos pos c  = let col = unPos $ sourceColumn pos
                    in pos { sourceColumn = mkPos (col + chars)}
                    where chars = case c of
                            '\t' -> 4 -- x = 4
                            _    -> 1 -- x = 1

-- advances position by string
advStr :: SourcePos -> String -> SourcePos
advStr pos str = foldl advPos pos str


data Token
    = TokenEnd      -- EOF
    | TokenDirective-- #include / #define
    | TokenInclArg  -- <whatever>
    | TokenSymbol   -- ^[A-Za-z_][A-Za-z0-9_]*$
    | TokenKeyWord  -- int,chat,float,double ...
    | TokenIntLit   -- 0,1,2,...
    | TokenSciLit   -- 10e10
    | TokenHexLit   -- 0x001, 0xFF, ...
    | TokenFloatLit -- 0.11, 32.14, ...
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
    | TokenStrLit   -- "whatever"
    | TokenCharLit  -- 'x'
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
    deriving (Show, Eq, Ord)



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
readLit :: Char -> SourcePos -> String -> (String, String, SourcePos)
readLit end = go []
  where
    go acc p [] = (reverse acc, [], p)
    go acc p (x:xs)
      | x == '\\' = case xs of
          []     -> go ('\\':acc) (advPos p x) []
          (y:ys) -> let p' = advPos p x in go (y:'\\':acc) (advPos p' y) ys
      | x == end = let p' = advPos p x in (reverse (x:acc), xs, p')
      | otherwise = go (x:acc) (advPos p x) xs

-- takes a str and returns function that goes till it finds that str
-- then returns whole symbol and rest
readLitStr :: String -> SourcePos -> String -> (String, String, SourcePos)
readLitStr end startPos str = go [] startPos str
  where
    n = length end
    go acc p [] = (reverse acc, [], p)
    go acc p s@(x:xs)
      | end `isPrefixOf` s = 
          let p' = advStr p end 
          in (reverse acc ++ end, drop n s, p')
      | otherwise = go (x:acc) (advPos p x) xs

-- Helper to emit a token and advance recursion seamlessly
emit :: Token -> String -> SourcePos -> String -> [LocatedToken]
emit tok text pos rest = 
    LToken tok pos text : lexe (advStr pos text) rest

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

lexe :: SourcePos -> String -> [LocatedToken]
lexe pos [] = [LToken TokenEnd pos ""]
lexe pos s@(x:xs)
    -- white spaces ' ','\t','\r'
    | x `elem` [' ','\t','\r'] = lexe (advPos pos x) xs
    -- preprocessor directives
    | x == '#' =
        let (lit, rest, newPos) = readLit '\n' pos xs
        in LToken TokenDirective pos (x : lit) : lexe newPos rest
    -- chars literals
    | x == '\'' =
        let (raw, rest, newPos) = readLit '\'' pos xs
        in LToken TokenCharLit   pos (x:raw)   : lexe newPos rest
    -- string literals
    | x == '"' =
        let (raw, rest, newPos) = readLit '"' pos xs
        in LToken TokenStrLit    pos (x:raw)   : lexe newPos rest
    -- hex numbers (prefix is 0x)
    | isHexStart s =
        let (hex, rest) = extractHex s
        in emit TokenHexLit ("0x"++hex) pos rest
    | x == '/' = case xs of
        -- inline comments
        ('/':_) -> let
                (_, rest, newPos) = readLit '\n' pos xs -- consume comment content
                fullText = '/' : take (length s - length rest - 1) xs -- reconstruct raw text
                in LToken TokenComment pos fullText : lexe newPos rest

        -- multiline comments
        ('*':_) -> let
                (lit, rest, newPos) = readLitStr "*/" (advPos pos '/') xs
                fullText = '/' : lit
                in LToken TokenComment pos fullText : lexe newPos rest
        -- /= operator
        ('=':ys) -> emit TokenDivEq "/=" pos ys
        _        -> emit TokenDivide "/" pos xs
    -- operators
    | x == '^' = case xs of
                    ('=':ys)     -> emit TokenXorEq  "^="  pos ys
                    _            -> emit TokenBitXor "^"   pos xs
    | x == '%' = case xs of
                    ('=':ys)     -> emit TokenModEq  "%="  pos ys
                    _            -> emit TokenMod    "%"   pos xs
    | x == '>' = case xs of
                    ('>':'=':ys) -> emit TokenRSEq   ">>=" pos ys
                    ('>':ys)     -> emit TokenRShift ">>"  pos ys
                    ('=':ys)     -> emit TokenGeq    ">="  pos ys
                    _            -> emit TokenGreater">"   pos xs
    | x == '<' = case xs of 
                    ('<':'=':ys) -> emit TokenLSEq   "<<=" pos ys
                    ('<':ys)     -> emit TokenLShift "<<"  pos ys
                    ('=':ys)     -> emit TokenLeq    "<="  pos ys
                    _            -> emit TokenLess   "<"   pos xs
    | x == '-' = case xs of
                    ('-':ys)     -> emit TokenDminus "--"  pos ys
                    ('>':ys)     -> emit TokenArrow  "->"  pos ys
                    ('=':ys)     -> emit TokenMeq    "-="  pos ys
                    _            -> emit TokenMinus  "-"   pos xs
    | x == '+' = case xs of
                    ('+':ys)     -> emit TokenDplus  "++"  pos ys
                    ('=':ys)     -> emit TokenPeq    "+="  pos ys
                    _            -> emit TokenPlus   "+"   pos xs
    | x == '&' = case xs of
                    ('&':ys)     -> emit TokenAnd    "&&"  pos ys
                    ('=':ys)     -> emit TokenAndEq  "&="  pos ys
                    _            -> emit TokenBitAnd "&"   pos xs
    | x == '|' = case xs of
                    ('|':ys)     -> emit TokenOr     "||"  pos ys
                    ('=':ys)     -> emit TokenOrEq   "|="  pos ys
                    _            -> emit TokenBitOr  "|"   pos xs
    | x == '!' = case xs of
                    ('=':ys)     -> emit TokenNeq    "!="  pos ys
                    _            -> emit TokenNot    "!"   pos xs
    | x == '*' = case xs of
                    ('=':ys)     -> emit TokenSEq    "*="  pos ys
                    _            -> emit TokenStar   "*"   pos xs
    | x == '=' = case xs of
                    ('=':ys)     -> emit TokenDeq    "=="  pos ys
                    _            -> emit TokenEq     "="   pos xs

    -- floats starting with .
    | x == '.' && maybe False (isNum . fst) (uncons xs) = let
        ( _ , rest) = span isNum xs
        r = case rest of
            ('f':rs) -> rs
            ('F':rs) -> rs
            _        -> rest
        consumed = take (length s - length r) s
        in emit TokenFloatLit consumed pos r
    -- numbers
    | isNum x = let
        ( _ , rest) = span isNum xs
        (kind, toParse) = case rest of
            -- Floats
            ('.': ys ) -> (TokenFloatLit, r)
               where ( _ , restF) = span isNum ys
                     r = case restF of ('f':zs)->zs; ('F':zs)->zs; _->restF
            -- Scientific notation
            ('e': y: ys ) | (isNum y || y=='-') -> (TokenSciLit, r)
               where ( _ , restS) = span isNum ys
                     r = case restS of ('f':zs)->zs; ('F':zs)->zs; _->restS
            -- Normal ints
            _ -> (TokenIntLit, rest')
               where ( _ , rest') = consumeIntSuffix rest

        -- Crucial: reconstruct original text to ensure correct position advancement
        consumedRaw = take (length s - length toParse) s
        in emit kind consumedRaw pos toParse

    -- symbols
    | isSymbolStart x = let
        (sym, rest) = span isSymbolMid xs
        full = x : sym
        tokenKind | isKeyWord full = TokenKeyWord
                  | otherwise      = TokenSymbol
        in emit tokenKind full pos rest

    -- other one char tokens and Invalid Token
    | otherwise =
        let tk = tokenKind x
            txt = [x]
        in if tk == TokenInvalid
            then [LToken TokenInvalid pos txt]
            else emit tk txt pos xs
     where
       tokenKind c
           | c == ';' = TokenSemi     | c == '(' = TokenParL    | c == ')' = TokenParR
           | c == '{' = TokenBraL     | c == '}' = TokenBraR    | c == '[' = TokenSqrL
           | c == ']' = TokenSqrR     | c == ',' = TokenComma   | c == '.' = TokenDot
           | c == ':' = TokenDblDot   | c == '?' = TokenTernary | c == '~' = TokenTilda
           | c == '\n' = TokenNewLine | otherwise = TokenInvalid

-- PUBILC API
-- wraper for reading from file
lexer :: String -> IO [LocatedToken]
lexer fileName = do
    fileContent <- readFile fileName
    let startPos = initialPos fileName
    return $ lexe startPos fileContent
