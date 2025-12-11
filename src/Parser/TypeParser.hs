module Parser.TypeParser where


import Text.Megaparsec hiding (Token)

import Parser.Common
import Parser.AST

parseType :: Parser Type
parseType = choice
    [ TInt   <$ matchText "int"
    , TVoid  <$ matchText "void"
    , TChar  <$ matchText "char"
    , TFloat <$ matchText "float"
    ]

