{-# LANGUAGE FlexibleInstances #-}
module Common where

import qualified Text.Megaparsec as TM
import qualified Data.List.NonEmpty as NE
import Data.List (isPrefixOf)

import Text.Megaparsec.Pos (
        SourcePos, unPos,
        sourceLine,sourceName,
        mkPos, sourceColumn)

data LocatedToken = LToken {
    lToken :: Token,    -- Token
    lPos   :: SourcePos,-- Where the Token is starting (file, line, column)
    lText  :: String    -- Original Token Text eg. ("  int   ")
} deriving (Show, Eq, Ord)

spliceLines :: String -> String
spliceLines [] = []
spliceLines ('\\':'\n':xs) = spliceLines xs
spliceLines (x:xs) = x : spliceLines xs

instance TM.VisualStream [LocatedToken] where
    showTokens   _ = unwords . map (\t -> case lToken t of
                                            TokenSpace   -> "\" \" (space)"
                                            _ -> "\"" ++ lText t ++ "\"") . NE.toList
    tokensLength _ = NE.length

instance TM.TraversableStream [LocatedToken] where
    reachOffset targetOffset pst = let
            (pre, post) = splitAt (targetOffset - TM.pstateOffset pst) (TM.pstateInput pst)
            newSourcePos = case post of
                (x:_) -> lPos x
                []    -> case reverse pre of
                           []    -> TM.pstateSourcePos pst
                           (x:_) -> let pos = lPos x
                                        col = unPos (sourceColumn pos)
                                        len = length (lText x)
                                    in pos { sourceColumn = mkPos (col + len) }

            targetLine = sourceLine newSourcePos
            onSameLine t = sourceLine (lPos t) == targetLine
            reconstructedLine = concatMap lText (filter onSameLine (pre ++ post))
        in
            (Just reconstructedLine, pst {
                TM.pstateInput = post,
                TM.pstateOffset = targetOffset,
                TM.pstateSourcePos = newSourcePos
            })

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
advStr = foldl advPos

data Token
    = TokenEnd      -- EOF
    | TokenSpace    -- ' '
    | TokenTab      -- '\t'
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
    | TokenStrLit   -- "whatever"
    | TokenCharLit  -- 'x'
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
    | TokenSlashR   -- /
    | TokenDivEq    -- /=
    | TokenMod      -- %
    | TokenModEq    -- %=
    | TokenDblDot   -- :  (goto labels, and ternary operator)
    | TokenTernary  -- ?
    | TokenTilda    -- ~

    | TokenInvalid  -- invalid token -> error
    deriving (Eq,Show ,Ord)

isNum :: Char -> Bool
isNum c = c `elem` ['0'..'9']

isHex :: Char -> Bool
isHex c = isNum c || c `elem` ['A'..'F'] || c `elem` ['a'..'f']

isAlpha :: Char -> Bool
isAlpha c
    | c `elem` ['A'..'Z'] = True
    | c `elem` ['a'..'z'] = True
    | otherwise           = False

isAlpnum :: Char -> Bool
isAlpnum c = isAlpha c || isNum c

isSymbolStart :: Char -> Bool
isSymbolStart c = isAlpha c || c == '_'

isSymbolMid :: Char -> Bool
isSymbolMid c = isAlpnum c || c == '_'

readTill :: String -> String -> (String, String)
readTill end = go []
    where
        n = length end
        go acc [] = (reverse acc, [])
        go acc s@(x:xs)
            | end `isPrefixOf` s = (reverse acc ++ end, drop n s)
            | otherwise = go (x:acc) xs

postProcess :: [LocatedToken] -> [LocatedToken]
postProcess = combineStringLiterals

combineStringLiterals :: [LocatedToken] -> [LocatedToken]
combineStringLiterals [] = []
combineStringLiterals (t:ts)
    | lToken t == TokenStrLit =
        case findNextStr ts of
            Just (nextStr, rest) ->
                let newText = init (lText t) ++ tail (lText nextStr)
                    newTok  = t { lText = newText }
                in combineStringLiterals (newTok : rest)
            Nothing -> t : combineStringLiterals ts
    | otherwise = t : combineStringLiterals ts
  where
    findNextStr :: [LocatedToken] -> Maybe (LocatedToken, [LocatedToken])
    findNextStr [] = Nothing
    findNextStr (x:xs)
        | lToken x == TokenStrLit = Just (x, xs)
        | lToken x `elem` [TokenSpace, TokenTab] = findNextStr xs
        | otherwise = Nothing
