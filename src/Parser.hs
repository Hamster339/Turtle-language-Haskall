module Parser where
import Ast
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Graphics.Gloss.Data.Color

{- This file contains the parser, which uses Parsec to parse a MiniTurtle
 - program into the AST. -}

-- Parser type: a Parsec parser, operating on strings, with no state
type Parser a = Parsec String () a

-- Two examples provided: parsing a colour and an integer
color :: Parser Color
color =
    (reserved lexer "red" >> return red)
    <|> (reserved lexer "green" >> return green)
    <|> (reserved lexer "blue" >> return blue)
    <|> (reserved lexer "yellow" >> return yellow)
    <|> (reserved lexer "black" >> return black)

int :: Parser Int
int = do
    i <- integer lexer
    return (fromIntegral i)

eInt :: Parser Expr
eInt = do
    i <- int
    return (EInt i)

bool :: Parser Expr
bool =
    (reserved lexer "true" >> return(EBool True))
    <|> (reserved lexer "false" >> return (EBool False))

unit :: Parser Expr
unit = char '(' >> char ')' >> (return EUnit)

type' :: Parser Type
type' =
    (reserved lexer "Int" >> return TyInt)
    <|> (reserved lexer "Bool" >> return TyBool)
    <|> (reserved lexer "Unit" >> return TyUnit)

var :: Parser Expr
var = do
    v <- identifier lexer
    return (EVar v)

changeColor :: Parser Expr
changeColor = do
    reserved lexer "changeColor"
    col <- (parens lexer color)
    return (EChangeColor col)

forward :: Parser Expr
forward = do
    reserved lexer "forward"
    ex <- (parens lexer expression)
    return (EMove Forward ex)

backward :: Parser Expr
backward = do
    reserved lexer "backward"
    ex <- (parens lexer expression)
    return (EMove Backward ex)

left :: Parser Expr
left = do
    reserved lexer "left"
    ex <- (parens lexer expression)
    return (ERotate RotateLeft ex)

right :: Parser Expr
right = do
    reserved lexer "right"
    ex <- (parens lexer expression)
    return (ERotate RotateRight ex)

penUp :: Parser Expr
penUp = do
    reserved lexer "penUp"
    unit
    return (EPenUp)

penDown :: Parser Expr
penDown = do
    reserved lexer "penDown"
    unit
    return (EPenDown)

arg_list :: Parser [Expr]
arg_list = (commaSep lexer expression)

ifStatement :: Parser Expr
ifStatement = do
    reserved lexer "if"
    ex1 <- (parens lexer expression)
    ex2 <- block
    reserved lexer "else"
    ex3 <- block
    return (EIf ex1 ex2 ex3)

block :: Parser Expr
block = (braces lexer (lexeme lexer expression))

letBinding :: Parser Expr
letBinding = do
    reserved lexer "let"
    v <- identifier lexer
    reservedOp lexer "="
    ex1 <- expression
    reserved lexer "in"
    ex2 <- expression
    return (ELet v ex1 ex2)

functionCall :: Parser Expr
functionCall = do
    name <- identifier lexer
    params <- (parens lexer arg_list)
    return (EApp name params)

action :: Parser Expr
action =
  changeColor
  <|> forward
  <|> backward
  <|> left
  <|> right
  <|> penUp
  <|> penDown

{- Exercise 4 -}
expr :: Parser Expr
expr =
  (try unit)
  <|> bool
  <|> eInt
  <|> action
  <|> try functionCall
  <|> ifStatement
  <|> letBinding
  <|> var
  <|> (parens lexer expression)

param :: Parser (Parameter,Type)
param = do
    v <- identifier lexer
    colon lexer
    t <- type'
    return ((v,t))

param_list :: Parser [(Parameter,Type)]
param_list = (commaSep lexer param)

defname :: Parser DefName
defname = identifier lexer

def :: Parser (DefName,Definition)
def = do
    reserved lexer "def"
    v <- defname
    params <- parens lexer param_list
    colon lexer
    t <- type'
    ex <- block
    return (v,(params, t, ex))

program :: Parser Program
program = do
    deflist <- many def
    ex <- expression
    return ((deflist, ex))

{- The remainder of the file is boilerplate which we have set up for you.
 - This includes setting up the expression parser, which means you do not
 - need to worry about unary and binary operators. -}
keywords = [
        "forward", "backward", "left", "right",
        "penUp", "penDown", "changeColor", "red",
        "green", "blue", "yellow", "black", "if", "else",
        "let", "in", "true", "false", "def","Int", "Bool", "Unit"
    ]

ops = [
        "+", "-", "*", "/", "==", "!=", ">", "<",
        ">=", "<=", "&&", "||"
    ]

langDef :: LanguageDef ()
langDef = emptyDef {
        commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        identStart = letter <|> (char '_'),
        identLetter = alphaNum <|> char '_',
        opStart = oneOf ";!&*+/<=>|-",
        opLetter = oneOf "&/=|",
        reservedNames = keywords,
        reservedOpNames = ops,
        caseSensitive = True
    }

lexer :: TokenParser ()
lexer = makeTokenParser langDef

binary  name fun assoc =
    Infix ( do { reservedOp lexer name; return fun }) assoc

prefix  name fun =
    Prefix (do { reservedOp lexer name; return fun })

postfix name fun =
    Postfix (do{ reservedOp lexer name; return fun })

table =
    [  [prefix "-" (EUnOp Neg), prefix "!" (EUnOp Not) ]
       , [binary "*" (EBinOp Mul) AssocLeft, binary "/" (EBinOp Div) AssocLeft ]
       , [binary "+" (EBinOp Add) AssocLeft, binary "-" (EBinOp Sub) AssocLeft ]
       , [ binary "<" (EBinOp Less) AssocLeft, binary "<=" (EBinOp LessEq) AssocLeft,
           binary ">" (EBinOp Greater) AssocLeft, binary ">=" (EBinOp GreaterEq) AssocLeft,
           binary "==" (EBinOp Eq) AssocLeft, binary "!=" (EBinOp Neq) AssocLeft
         ]
       , [binary "&&" (EBinOp And) AssocLeft, binary "||" (EBinOp Or) AssocLeft]
       , [binary ";" ESeq AssocRight]
    ]

expression :: Parser Expr
expression = buildExpressionParser table expr

-- External definition, which runs the parser
parse :: String -> String -> Either ParseError Program
parse = runP program ()
