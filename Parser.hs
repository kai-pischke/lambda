module Parser (Parser, eat, eatspaces, many, some, run, parens, chainl1, (<|>)) where 

import Control.Applicative
import Control.Monad
import Data.Char 

newtype Parser a = Parser (String -> [(a, String)])

run :: Parser a -> String -> [(a, String)]
run (Parser pa) = pa

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f pa = Parser $ \s -> [(f a, s') | (a, s') <- run pa s]
                                 

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure a = Parser $ \s -> [(a, s)]
    
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b 
    pf <*> pa = Parser $ \s -> concatMap (\(f, s') -> run (fmap f pa) s') (run pf s)
                                
instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = Parser $ \s -> concatMap (\(a, s') -> run (f a) s') (run pa s)
                        
instance Alternative Parser where  
    -- empty :: Parser a
    empty = Parser $ \s -> []
    
    -- (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = Parser $ \s -> case run pa s of 
        [] -> run pb s
        xs -> xs

instance MonadPlus Parser where
    -- mzero :: Parser a
    mzero = empty 
    
    -- mplus :: Parser a -> Parser a -> Parser a
    mplus pa pb = Parser $ \s -> run pa s ++ run pb s

            
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 pa pc = pa >>= rest
    where rest x = more x <|> return x
          more x = do
            c <- pc
            y <- pa
            rest (x `c` y)
    
nibble :: Parser Char
nibble = Parser $ \s -> case s of 
    "" -> []
    (x:xs) -> [(x, xs)]

eatif :: (Char -> Bool) -> Parser Char 
eatif f = do 
    x <- nibble 
    if f x then return x else empty
    
eatchar :: Char -> Parser Char
eatchar c = eatif (==c)

eatalpha :: Parser Char 
eatalpha = eatif (`elem` (['a'..'z'] ++ ['A'..'Z']))

eatstring :: String -> Parser String 
eatstring "" = return ""
eatstring (x:xs) = do 
    c <- eatchar x
    cs <- eatstring xs
    return (c:cs) 

eatspace :: Parser Char 
eatspace = eatif isSpace

eatspaces :: Parser String
eatspaces = many eatspace

parens :: Parser a -> Parser a
parens pa = eat "(" *> pa <* eat ")"
    
eatdigit :: Parser Char
eatdigit = eatif isDigit

eatint :: Parser Int
eatint = eatspaces >> fmap read (some eatdigit)
            
eat :: String -> Parser String 
eat s = eatspaces >> eatstring s