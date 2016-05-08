{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Control.Applicative

-- |My notes whilst reading Monadic Parser Combinators by Graham Hutton and Eric Meijer

-- |In the paper, they simply use the type declaration
--
--    type Parser a = String -> [(a, String)]
--
--  but this would not compile in Haskell, so I reformed it like they suggested into a
--  data declaration such that it would work and I could actually play around.
--  The only things I'm not too positive of are the implementations of Alternative,
--  but I can't imagine how else I would implement them.

data Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser f) = f

instance Functor Parser where
  fmap f action = do x <- action
                     return $ f x

instance Applicative Parser where
  pure v = Parser (\inp -> [(v, inp)])
  mf <*> ma = do f <- mf
                 a <- ma
                 return $ f a

instance Monad Parser where
  (Parser p) >>= f = Parser (\inp -> concat [parse (f v) inp' | (v, inp') <- p inp])
  return = pure

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = Parser (\inp -> [])
  (Parser p) `mplus` (Parser q) = Parser (\inp -> (p inp ++ q inp))

-- |Char eater
item :: Parser Char
item = Parser (\inp -> case inp of
                  []     -> []
                  (x:xs) -> [(x, xs)])

-- |Qualified item
sat :: (Char -> Bool) -> Parser Char
sat p = do 
  x <- item
  if p x
    then return x
    else mzero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

range :: Char -> Char -> Parser Char
range lo hi = sat (\x -> lo <= x && x <= hi)

-- |Parses a white space
ws :: Parser Char
ws = char ' '

-- |Parses a digit
digit :: Parser Char
digit = range '0' '9'

-- |Parses a lowercase letter
lower :: Parser Char
lower = range 'a' 'z'

-- |Parses an uppercase letter
upper :: Parser Char
upper = range 'A' 'Z'

-- |Parses any letter
letter :: Parser Char
letter = lower `mplus` upper

-- |Parses digits and letters
alphanum :: Parser Char
alphanum = letter `mplus` digit

-- |Parses a natural number
nat :: Parser String
nat = many digit

-- |Parses a word
word :: Parser String
word = many letter

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

string :: String -> Parser String
string "" = return ""
string (x:xs) = do 
   _ <- char x 
   _ <- string xs
   return (x:xs)
