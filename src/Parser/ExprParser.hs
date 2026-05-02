module Parser.ExprParser where

import Common
import Text.Megaparsec hiding (Token)
import Control.Monad (void)
import Control.Monad.Combinators.Expr

import Parser.Common
import Parser.AST

parseUnaryPrefix :: Parser UnaryOp
parseUnaryPrefix = choice
            [ Not           <$ readToken TokenNot
            , MinusSign     <$ readToken TokenMinus
            , FirstCompl    <$ readToken TokenTilda
            , IncrementPre  <$ readToken TokenDplus
            , DecrementPre  <$ readToken TokenDminus
            , AdressOf      <$ readToken TokenBitAnd
            , Deref         <$ readToken TokenStar
            ]

parseElement :: Parser Expression
parseElement = do
    void $ parseToken TokenSqrL
    expr <- parseExpr
    void $ parseToken TokenSqrR
    pure expr

parseRefField :: Parser String
parseRefField = do
    void $ parseToken TokenArrow
    readToken TokenSymbol

parseField :: Parser String
parseField = do
    void $ parseToken TokenDot
    readToken TokenSymbol

parseUnaryPostfix :: Parser UnaryOp
parseUnaryPostfix = choice
            [ IncrementPost  <$  readToken TokenDplus
            , DecrementPost  <$  readToken TokenDminus
            , RefField       <$> parseRefField
            , Field          <$> parseField
            , Element        <$> parseElement
            ]

parseSymbolOrCall :: Parser Expression
parseSymbolOrCall = do
    name <- readToken TokenSymbol
    isCall <- optional $ lookAhead $ parseToken TokenParL

    case isCall of
        Just _ -> do
            void $ parseToken TokenParL
            args <- parseExpr `sepBy` parseToken TokenComma
            void $ parseToken TokenParR
            pure $ FuncCall name args
        Nothing ->
            pure $ StringExp name

stripIntSuffix :: String -> String
stripIntSuffix = reverse . dropWhile (`elem` "uUlL") . reverse

stripFloatSuffix :: String -> String
stripFloatSuffix = reverse . dropWhile (`elem` "fFlL") . reverse

safeReadInt :: String -> Integer
safeReadInt s =
    let clean = stripIntSuffix s
    in if length clean > 1 && head clean == '0' && clean !! 1 /= 'x' && clean !! 1 /= 'X'
       then read ("0o" ++ drop 1 clean)
       else read clean

safeReadFloat :: String -> Float
safeReadFloat = read . stripFloatSuffix

-- atomic terms like 5, 0, (3+2), etc..
parseTerm :: Parser Expression
parseTerm = do
        unaryOpPre <- optional parseUnaryPrefix
        expr <- choice
            [ parens parseExpr
            , IntegerExp . safeReadInt   <$> (readToken TokenIntLit <|> readToken TokenHexLit)
            , FloatExp   . safeReadFloat <$> (readToken TokenFloatLit <|> readToken TokenSciLit)
            , CharExp    . (!!1) <$> readToken TokenCharLit
            , StringExp          <$> readToken TokenStrLit
            , parseSymbolOrCall
            ]
        unaryOpPost <- optional parseUnaryPostfix
        case (unaryOpPre, unaryOpPost) of
            (Just op, Nothing)    -> pure $ UnaryExp op expr
            (Nothing, Just op)    -> pure $ UnaryExp op expr
            (Just pre, Just post) -> pure $ UnaryExp pre $ UnaryExp post expr
            _                     -> pure expr

-- 1) ! ~ ++ -- (unary -) & * []
-- 2) * / %
-- 3) + -
-- 4) >> <<
-- 5) < <= > >=
-- 6) == !=
-- 7) & (bitwise)
-- 8) ^ (bitwise)
-- 9) | (bitwise)
-- 10) && (logic)
-- 11) || (logic)
-- 12) = (assignment) += -= *= /= %= <<= >>= &= ^= |=
operatorTable :: [[Operator Parser Expression]]
operatorTable =
    [ [ binary TokenStar    Multiplication , binary TokenSlashR Division,
        binary TokenMod     Modulo]
    , [ binary TokenPlus    Addition ,       binary TokenMinus  Substraction ]
    , [ binary TokenLShift  BitShiftL,       binary TokenRShift BitShiftR    ]
    , [ binary TokenLess    Less,            binary TokenLeq    Leq,
        binary TokenGreater Greater,         binary TokenGeq    Geq]
    , [ binary TokenDeq     Equal,           binary TokenNeq    NotEqual]
    , [ binary TokenBitAnd  BitAnd]
    , [ binary TokenBitXor  BitXor]
    , [ binary TokenBitOr   BitOr ]
    , [ binary TokenAnd     LogicAnd]
    , [ binary TokenOr      LogicOr]
    , [ InfixR parseAssignOp,                binaryR TokenPeq   PlusEq,
        binaryR TokenMeq    MinusEq,         binaryR TokenSEq   MulEq,
        binaryR TokenDivEq  DivEq,           binaryR TokenModEq ModEq,
        binaryR TokenLSEq   LShiftEq,        binaryR TokenRSEq  RShiftEq,
        binaryR TokenAndEq  AndEq,           binaryR TokenXorEq XorEq,
        binaryR TokenOrEq   OrEq
      ]
    ] where
        binary tok op = InfixL $ BinExp op <$ parseToken tok
        binaryR tok op = InfixR $ BinExp op <$ parseToken tok

parseAssignOp :: Parser (Expression -> Expression -> Expression)
parseAssignOp = AssigmentExp <$ parseToken TokenEq

parseExpr :: Parser Expression
parseExpr =
    makeExprParser parseTerm operatorTable
