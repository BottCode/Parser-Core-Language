module ParserCoreLanguage where
import Control.Applicative
import Control.Monad
import Data.Char
import System.IO

import Debug.Trace

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> [(g v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
        [] -> []
        [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
        [] -> []
        [(v,out)] -> parse (f v) out)

-- it removes the dummy constructor
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- it fails if input string is empty. Otherwhise succeeds with the first Char as the result value
item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)])

{-- remind Maybe as an Alternative
    instance Alternative Maybe where 
        -- empty :: Maybe a
        empty = Nothing

        -- (<|>) :: Maybe a -> Maybe a -> Maybe a
        Nothing <|> my = my
        (Just x) <|> _ = Just x
--}

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
              [] -> parse q inp
              [(v,out)] -> [(v,out)])


-- Derived Primitive
-- item, return and empty are three basic parser. In combination with sequencing and choice,
-- these primitive can be used to efine a number of other useful parsers

-- It returns a parser iff predicate p is true
-- parse (sat (=='a')) "abc"  =  [('a',"bc")]
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if (p x) 
               then return x 
               else empty

-- return a parser iff the first character is a number
-- parse digit "12abc"  =  [('1',"2abc")]
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

-- return a parser iff the string is at the beginning of the param
-- parse (string "ab") "abc"  =  [("ab","c")]
-- parse (string "ab") "aab"  =  []
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- parse a natural number
-- parse nat "18 * 23"  =  [(18," * 23")]
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- parse a space
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- HANDLING SPACING

-- it allows to ignore any space before and after applying a parser for a token
token :: Parser a -> Parser a
token p = do space
             v <- p 
             space 
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = do token (char '-')
             n <- token nat
             return (-n)
          <|> token nat

symbol :: String -> Parser String
symbol xs = token (string xs)


-- A core-language program is just a list of supercombinatoric definitions
type Program a = [ScDefn a]

type CoreProgram = Program Name

-- a supercombinatoric definition contains the name of the supercombinator, its arguments and its body
type ScDefn a = (Name,[a],Expr a)

type CoreScDefn = ScDefn Name

type Name = String

type Def a = (a, Expr a) -- for let and letrec

type Alter a = (Int, [a], Expr a) -- for case

-- in Expr using IsRec you use the constructor ELet for modelling both let and letrec
data IsRec = NonRecursive | Recursive
             deriving Show

data Expr a = EVar Name
            | ENum Int
            | EConstr Int Int
            | EAp (Expr a) (Expr a)
            | ELet
                IsRec
                [Def a]
                (Expr a)
            | ECase
                (Expr a)
                [Alter a]
            | ELam [a] (Expr a)
            deriving Show

-- parser per progamma
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

-- parser per supercombinator
parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- checkParseVar
                pf <- many checkParseVar
                char '='
                body <- parseExpr
                return (v, pf, body)
                
-- it is for the following cases: let, letrec, case, lambda, aexpr and infix binary application.
parseExpr :: Parser (Expr Name)
parseExpr = parseLet <|> parseLetRec <|> parseCase <|> parseLambda <|> parseExpr1

parseLet :: Parser (Expr Name) -- let is something like "let var1 = expr1; var2 = expr2; in expr0;"
parseLet = do symbol "let"
              defns <- some parseDef 
              symbol "in"
              e <- parseExpr
              return (ELet NonRecursive defns e)

parseLetRec :: Parser (Expr Name)
parseLetRec = do symbol "letrec"
                 defns <- some parseDef
                 symbol "in"
                 body <- parseExpr
                 return (ELet Recursive defns body)

-- Parse a "Definition". It's used by parseLet and parseLetRec for Def (let and letrec).
-- defn -> var = expr
parseDef :: Parser (Def Name)
parseDef = do x <- checkParseVar
              symbol "="
              expr <- parseAExpr
              return (x, expr)

parseCase :: Parser (Expr Name)
parseCase = do symbol "case"
               e <- parseExpr
               symbol "of"
               alts <- parseMoreAlts
               return (ECase e alts)

parseLambda :: Parser (Expr Name)   
parseLambda = do symbol "\\"
                 var <- some checkParseVar
                 symbol "."
                 e <- parseExpr
                 return (ELam var e)

parseMoreAlts :: Parser [(Alter Name)]
parseMoreAlts = do alt <- parseAlt
                   do symbol ";"
                      remaining_alts <- parseMoreAlts
                      return (alt:remaining_alts)
                      <|>
                      return [alt]

-- used by parseMoreAlts
parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              num <- integer
              symbol ">"
              var <- many checkParseVar
              symbol "->"
              e <- parseExpr
              return (num, var, e)

-- expr1 -> expr2 or expr1 | expr2
parseExpr1 :: Parser (Expr Name)
parseExpr1 = do left <- parseExpr2
                do symbol "|"
                   right <- parseExpr1
                   return (EAp (EAp (EVar "|") left) right)
                 <|> return left

-- expr2 -> expr3 & expr2 | expr3
parseExpr2 :: Parser (Expr Name)
parseExpr2 = do left <- parseExpr3
                do symbol "&"
                   right <- parseExpr2
                   return (EAp (EAp (EVar "&") left) right)
                 <|> return left

-- expr3 -> expr4 relop expr4 | expr4
parseExpr3 :: Parser (Expr Name)
parseExpr3 = do left <- parseExpr4
                do op <- parseRelOp
                   right <- parseExpr4
                   return (EAp (EAp (EVar op) left) right) 
                 <|> return left

-- used in parseExpr3
parseRelOp :: Parser Name
parseRelOp = symbol "<" <|> symbol "<=" <|> symbol "==" <|> symbol "~=" <|> symbol ">=" <|> symbol ">"

-- expr4 -> expr5 + expr4 | expr5 - expr5 | expr5
parseExpr4 :: Parser (Expr Name)
parseExpr4 = do left <- parseExpr5
                do symbol "+"
                   right <- parseExpr4
                   return (EAp (EAp (EVar "+") left) right)
                 <|> do symbol "-"
                        right <- parseExpr5
                        return (EAp (EAp (EVar "-") left) right)
                 <|> return left

-- expr5 -> expr6 * expr5 | expr6 / expr6 | expr6
parseExpr5 :: Parser (Expr Name)
parseExpr5 = do left <- parseExpr6
                do symbol "*"
                   right <- parseExpr5
                   return (EAp (EAp (EVar "*") left) right)
                 <|> do symbol "/"
                        right <- parseExpr6
                        return (EAp (EAp (EVar "/") left) right)
                 <|> return left

-- aexpr6 -> aexpr_1...aexpr_n
parseExpr6 :: Parser (Expr Name)
parseExpr6 = do x:xs <- some parseAExpr
                return (foldl EAp x xs)

parseAExpr :: Parser (Expr Name)
parseAExpr = parseVar <|> parseNum <|> parsePack <|> parseParenthesisedExpr

parseVar :: Parser (Expr Name)
parseVar = do var <- checkParseVar
              return (EVar var)

parseNum :: Parser (Expr Name)
parseNum = do num <- integer
              return (ENum num)     

parsePack :: Parser (Expr Name)
parsePack = do symbol "Pack"
               symbol "{"
               tag <- integer
               symbol ","
               arity <- integer
               symbol "}"
               return (EConstr tag arity)

parseParenthesisedExpr :: Parser (Expr Name)
parseParenthesisedExpr = do symbol "("
                            e <- parseExpr
                            symbol ")"
                            return e

checkParseVar :: Parser Name
checkParseVar = token ident

-- this is the actual variables parsing.
-- grammar: var -> alphaChar varch_1...varch_n
-- where varch_x is -> alphaChar | digit | _
ident :: Parser Name
ident = do x <- letter -- an alphabetic character
           xs <- many varch -- alpha | digit | _
           if isKeyWord (x:xs)
            then empty
            else return (x:xs)

isKeyWord :: String -> Bool
isKeyWord "in" = True
isKeyWord "of" = True
isKeyWord "case" = True
isKeyWord "let" = True
isKeyWord "letrec" = True
isKeyWord "Pack" = True
isKeyWord _ = False

varch :: Parser Char
varch = do alpha <- letter
           return alpha
        <|>
        do numeral <- digit
           return numeral
        <|>
        do underscore <- char '_'
           return underscore