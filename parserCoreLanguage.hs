module ParserCoreLanguage where

import Control.Applicative
import Control.Monad
import Data.Char
import System.IO

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

{--
-- it consumes three characters, discard the second, and returns the first and the third as a pair
three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)
--}

-- Monadic version
three :: Parser (Char,Char)
three = do x <- item 
           item
           z <- item
           return (x,z)

{-- reminf Maybe as an Alternative
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

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if (p x) 
               then return x 
               else empty

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

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int 
int = do char '-'
         n <- nat
         return (-n)
    <|> nat

-- HANDLING SPACING

token :: Parser a -> Parser a
token p = do space
             v <- p 
             space 
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- LET'S START WITH THE PARSER

type Program a = [ScDefn a]

type CoreProgram = Program Name

type ScDefn a = (Name,[a],Expr a)

type CoreScDefn = ScDefn Name

type Name = String

type Def a = (a, Expr a) -- for let and letrec

type Alter a = (Int, [a], Expr a) -- for case

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

{--parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do character ';'
                  ps <- parseProg
                  return (p:ps)
               <|> return [p]   

parseScDef :: Parser (ScDef Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                character '='
                body <- parseExpr -- call to parseExpr
                return (v,pf,body)

parseExpr :: Parser (Expr Name)

parseAExpr :: Parser (Expr Name)

parseDef :: Parser (Def Name)

parseAlt :: Parser (Alter Name)--}