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
--- Operador para combinar parsers
--- sin perder entrada
-----------------------------------

(<->) :: Parser a -> Parser a -> Parser a
(<->) p p' = (try p) <|> p'

----------------------------------
--- Parser de expressiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp = chainl1 intexp1 intexpSeq

intexp1 :: Parser (Exp Int)
intexp1 = intexpAssgn <-> intexp2

intexp2 :: Parser (Exp Int)
intexp2 = chainl1 intexp3 (intexpPlus <-> intexpMinus)

intexp3 :: Parser (Exp Int)
intexp3 = chainl1 intexp4 (intexpTimes <-> intexpDiv)

intexp4 :: Parser (Exp Int)
intexp4 = (parens lis intexp) <-> intexpUM <-> intexpConst <-> intexpVar

intexpAssgn :: Parser (Exp Int)
intexpAssgn = do v <- identifier lis
                 reservedOp lis "="
                 e <- intexp
                 return (EAssgn v e)

intexpSeq :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpSeq = do {reservedOp lis ","; return ESeq}

intexpConst :: Parser (Exp Int)
intexpConst = do {n <- natural lis; return (Const (fromInteger n))}

intexpVar :: Parser (Exp Int)
intexpVar = do {v <- identifier lis; return (Var v)}

intexpUM :: Parser (Exp Int)
intexpUM = do {reservedOp lis "-"; e <- intexp4; return (UMinus e)}

intexpTimes :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpTimes = do {reservedOp lis "*"; return Times}

intexpDiv :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpDiv = do {reservedOp lis "/"; return Div}

intexpPlus :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpPlus = do {reservedOp lis "+"; return Plus}

intexpMinus :: Parser (Exp Int -> Exp Int -> Exp Int)
intexpMinus = do {reservedOp lis "-"; return Minus}
                                    
-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 boolexp1 boolexpOr

boolexp1 :: Parser (Exp Bool)
boolexp1 = chainl1 boolexp2 boolexpAnd

boolexp2 :: Parser (Exp Bool)
boolexp2 = boolexpNot <-> boolexp3

boolexp3 :: Parser (Exp Bool)
boolexp3 = (parens lis boolexp) <-> boolexpBool <-> boolexpOp

boolexpBool :: Parser (Exp Bool)
boolexpBool = do reserved lis "true"
                 return BTrue
                 <-> (do reserved lis "false"
                         return BFalse)

boolexpNot :: Parser (Exp Bool)
boolexpNot = do reservedOp lis "!"
                b <- boolexp2
                return (Not b)

boolexpOp :: Parser (Exp Bool)
boolexpOp = do e <- intexp
               op <- (try boolexpEq <-> boolexpNEq <-> boolexpGt <-> boolexpLt)
               e' <- intexp
               return (op e e')

boolexpOr :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolexpOr = do {reservedOp lis "||"; return Or}

boolexpAnd :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
boolexpAnd = do {reservedOp lis "&&"; return And}

boolexpEq :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolexpEq = do {reservedOp lis "=="; return Eq}

boolexpNEq :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolexpNEq = do {reservedOp lis "!="; return NEq}

boolexpGt :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolexpGt = do {reservedOp lis ">"; return Gt}

boolexpLt :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolexpLt = do {reservedOp lis "<"; return Lt}

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = commSeq

comm1 :: Parser Comm
comm1 = commVar <-> commIf <-> commWhile <-> commSkip

commSkip :: Parser Comm
commSkip = do reserved lis "skip"
              return Skip

commVar :: Parser Comm
commVar = do v <- identifier lis
             reservedOp lis "="
             e <- intexp
             return (Let v e)

commSeq :: Parser Comm
commSeq = chainl1 comm1 (do {reservedOp lis ";"; return Seq})

commIf :: Parser Comm
commIf = do reserved lis "if"
            b <- boolexp
            c <- braces lis comm
            (do reserved lis "else"
                c' <- braces lis comm
                return (IfThenElse b c c')
                <-> return (IfThen b c))

commWhile :: Parser Comm
commWhile = do reserved lis "while"
               b <- boolexp
               c <- braces lis comm
               return (While b c)

------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)