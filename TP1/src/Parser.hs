module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp = chainl1 (try intexpConst <|> intexpVar <|> intexpUM) (try intexpPlus <|> intexpMinus)                           

intexpConst :: Parser (Exp Int)
intexpConst = do n <- natural lis
                 return (Const (fromInteger n))

intexpVar :: Parser (Exp Int)
intexpVar = do v <- identifier lis
               return (Var v)

intexpUM :: Parser (Exp Int)
intexpUM = do reservedOp lis "-"
              e <- intexp
              return (UMinus e)

intexpTimes :: Parser (Exp Int)
intexpTimes = do {reservedOp lis "+"; return (\e e' -> (Plus e e'))}

intexpDiv :: Parser (Exp Int)
intexpDiv = do {reservedOp lis "/"; return (\e e' -> (Div e e'))}

intexpPlus :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpPlus = do {reservedOp lis "+"; return (\e e' -> (Plus e e'))}

intexpMinus :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpMinus = do {reservedOp lis "-"; return (\e e' -> (Minus e e'))}
                                    
-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = return BTrue

-----------------------------------
--- Parser de comandos
-----------------------------------

commSkip :: Parser Comm
commSkip = do reserved lis "skip"
              return Skip

commVar :: Parser Comm
commVar = do v <- identifier lis
             reservedOp lis "="
             e <- intexp
             return (Let v e)

commSeq :: Parser Comm
commSeq = chainl1 comm (do {reservedOp lis ";"; return (\c c' -> (Seq c c'))})

commIf :: Parser Comm
commIf = do reserved lis "if"
            b <- boolexp
            char '{'
            c <- comm
            char '}'
            (do try (do reserved lis "else"
                        char '{'
                        c' <- comm
                        char '}'
                        return (IfThenElse b c c'))
                <|> return (IfThen b c))

comm :: Parser Comm
comm = do commSkip
          <->
          commVar
          <->
          commIf
          <->
          commSeq

------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

(<->) :: Parser a -> Parser a -> Parser a
(<->) x y = (try x) <|> y