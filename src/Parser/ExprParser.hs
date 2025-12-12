module Parser.ExprParser where

import Lexer
import Text.Megaparsec hiding (Token)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Void

import Parser.Common
import Parser.AST

parseUnaryPrefix :: Parser UnaryOp
parseUnaryPrefix = choice
            [ Not           <$ readToken TokenNot
            , MinusSign     <$ readToken TokenMinus
            , FirstCompl    <$ readToken TokenTilda
            , IncrementPre  <$ readToken TokenDplus
            , DecrementPre  <$ readToken TokenDminus
            ]

parseUnaryPostfix :: Parser UnaryOp
parseUnaryPostfix = choice
            [ IncrementPost  <$ readToken TokenDplus
            , DecrementPost  <$ readToken TokenDminus
            ]
            
parseSymbolOrCall :: Parser Expr
parseSymbolOrCall = do
    name <- readToken TokenSymbol
    isCall <- optional $ lookAhead $ parseToken TokenParL
    
    case isCall of
        Just _ -> do
            void $ parseToken TokenParL
            args <- parseExpr `sepBy` parseToken TokenComma
            void $ parseToken TokenParR
            return $ FuncCall name args
        Nothing -> 
            return $ SExp name

-- atomic terms like 5, 0, (3+2), etc..
parseTerm :: Parser Expr
parseTerm = do
        skipWhitespace
        unaryOpPre <- optional parseUnaryPrefix
        expr <- choice
            [ parens parseExpr
            , IExp . read  <$> readToken TokenIntLit
            , FExp . read  <$>(readToken TokenFloatLit
                           <|> readToken TokenSciLit)
            , CExp . (!!1) <$> readToken TokenCharLit
            , parseSymbolOrCall
            ]
        unaryOpPost <- optional parseUnaryPostfix
        skipWhitespace
        case (unaryOpPre, unaryOpPost) of
            (Just op, Nothing)    -> pure $ UnaryExp op expr
            (Nothing, Just op)    -> pure $ UnaryExp op expr
            (Just pre, Just post) -> pure $ UnaryExp pre $ UnaryExp post expr
            _                     -> pure expr

-- 1) ! ~ ++ -- (unary -)
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
type BaseParser = Parsec Void Input
operatorTable :: [[Operator BaseParser Expr]]
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
    , [ InfixR parseAssignOp,                binary TokenPeq    PlusEq,
        binary TokenMeq     MinusEq,         binary TokenSEq    MulEq,
        binary TokenDivEq   DivEq,           binary TokenModEq  ModEq,
        binary TokenLSEq    LShiftEq,        binary TokenRSEq   RShiftEq,
        binary TokenAndEq   AndEq,           binary TokenXorEq  XorEq,
        binary TokenOrEq    OrEq
      ]
    ] where
        binary tok op = InfixL $ BinExp   op <$ parseToken tok

parseAssignOp :: Parser (Expr -> Expr -> Expr)
parseAssignOp = do
    void $ parseToken TokenEq
    return $ \lhs rhs ->
        case lhs of
            SExp s -> Assign s rhs
            _      -> error "L-value is required for assignment"

parseExpr :: Parser Expr
parseExpr =
    makeExprParser parseTerm operatorTable
