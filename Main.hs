module Main where
import Parser
import Syntax
import Lambda
import System.IO

nIter :: Int
nIter = 1000

data Command = HeadNormal Term
             | BetaNormal Term
             | LeftMost Term
             | DisplayDeBruijn Term
             | Display Term deriving Show

read' :: IO String
read' = putStr ">> " >> hFlush stdout >> getLine

parseCommand :: Parser Command
parseCommand = parseHN <|> parseN <|> parseLM <|> parseDB <|> parseDisplay
    where
        parseHN = do 
            eat ":hn " <|> eat ":headnormal "
            t <- parseTerm
            return $ HeadNormal t
        parseN = do 
            eat ":bn " <|> eat ":betanormal "
            t <- parseTerm
            return $ BetaNormal t
        parseLM = do 
            eat ":lm " <|> eat ":leftmost "
            t <- parseTerm
            return $ LeftMost t
        parseDB = do 
            eat ":db " <|> eat ":debruijn "
            t <- parseTerm
            return $ DisplayDeBruijn t
        parseDisplay = Display <$> parseTerm

eval :: Command -> String
eval (Display t) = show $ unDeBruijnify $ deBruijnify t 
eval (DisplayDeBruijn t) = show $ deBruijnify t 
eval (LeftMost t) = case leftmost $ deBruijnify t of
    Nothing -> "(β-normal) " ++ show (unDeBruijnify $ deBruijnify t)
    Just t' -> show $ unDeBruijnify t'
eval (BetaNormal t) = case betanormal nIter $ deBruijnify t of
    OutOfIterations  -> "Couldn't find normal form after " ++ show nIter ++ " iterations.\nTerm may not be normalisable."
    Unnormalizable -> "Term isn't normalisable."
    TermTooBig -> "Probably not normalisable (maximum size exceeded during computation). "
    BNormal t' -> show $ unDeBruijnify t'
eval x = show x
 
evalString :: String -> String
evalString s = case run (parseCommand <* eatspaces) s of
    ((c,[]):_) -> eval c
    _ -> "parse error :("

main :: IO ()
main = loop
    where 
    loop = do
            input <- read'
            if input == ":quit" || input == ":q"
            then putStrLn "bye!" >> return ()
            else putStrLn (evalString input) >> loop


