module Syntax where
import Parser
import Data.Set (Set, union, empty, singleton, (\\))
import qualified Data.Set as Set

data Term = Var Variable
          | Abs Variable Term
          | App Term Term

data DeBruijn = DVar Int
              | DAbs DeBruijn
              | DApp DeBruijn DeBruijn deriving Eq

data Variable = A | B | C | D | E | F | G | H | I | J | K | L | M 
              | N | O | P | Q | R | S | T | U | V | W | X | Y | Z deriving (Bounded, Enum, Ord, Eq)

instance Show Variable where
    show A = "a"
    show B = "b"
    show C = "c"
    show D = "d"
    show E = "e"
    show F = "f"
    show G = "g"
    show H = "h"
    show I = "i"
    show J = "j"
    show K = "k"
    show L = "l"
    show M = "m"
    show N = "n"
    show O = "o"
    show P = "p"
    show Q = "q"
    show R = "r"
    show S = "s"
    show T = "t"
    show U = "u"
    show V = "v"
    show W = "w"
    show X = "x"
    show Y = "y"
    show Z = "z"
    
instance Show Term where
    show (Var v) = show v
    show t@(App _ _) = concat $ reverse $ appchain t
        where 
            appchain :: Term -> [String]
            appchain (App t1 t2) = atom t2 : appchain t1 
            appchain nonapp = [atom nonapp]
    show t@(Abs _ _) = "λ" ++ lamdachain t
        where 
            lamdachain :: Term -> String
            lamdachain (Abs v t1) = show v ++ lamdachain t1 
            lamdachain nonabs = "." ++ show nonabs

instance Show DeBruijn where
    show (DVar i) = show i
    show t@(DApp _ _) = concat $ reverse $ appchain t
        where 
            appchain :: DeBruijn -> [String]
            appchain (DApp t1 t2) = (" " ++ deBruijnAtom t2) : appchain t1 
            appchain nonapp = [deBruijnAtom nonapp]
    show (DAbs t) = "λ " ++ show t 

iComb = (Abs A (Var A))
sComb = (Abs A (Abs B (Abs C (App (App (Var A) (Var C)) (App (Var B) (Var C))))))
kComb = (Abs A (Abs B (Var A))) 
tComb = (Abs A (Abs B (Var A)))
fComb = (Abs A (Abs B (Var B)))
yComb = (Abs A (App middle middle))
    where middle = (Abs B (App (App (Var A) (Var B)) (Var B)))
omegaComb = App w w
    where w = (Abs A (App (Var A) (Var A)))

readTerm :: String -> Maybe Term
readTerm s = case run (parseTerm <* eatspaces) s of 
    ((a,[]):_) -> Just a
    _ -> Nothing

unsafeRead :: String -> Term
unsafeRead s = case readTerm s of 
    Nothing -> error "parse error when reading lambda term"
    Just a -> a

atom :: Term -> String
atom (Var v) = show v
atom t = "(" ++ show t ++ ")"

deBruijnAtom :: DeBruijn -> String
deBruijnAtom (DVar i) = show i
deBruijnAtom t = "(" ++ show t ++ ")"

deBruijnify :: Term -> DeBruijn
deBruijnify = convert 0
    where 
        rebind :: Int -> Int -> DeBruijn -> DeBruijn
        rebind goal target t@(DVar k) = if k == target then (DVar goal) else t
        rebind goal target (DApp t1 t2) = DApp (rebind goal target t1) (rebind goal target t2)
        rebind goal target (DAbs t) = DAbs $ rebind (goal + 1) (target + 1) t

        convert :: Int -> Term -> DeBruijn
        convert d (Var v) = DVar $ d + fromEnum v
        convert d (Abs v t) = DAbs $ rebind 0 (d + fromEnum v + 1) (convert (d+1) t)
        convert d (App t1 t2) = DApp (convert d t1) (convert d t2)

unDeBruijnify :: DeBruijn -> Term
unDeBruijnify t = convert 0 [] notfree t
    where
        notfree :: [Variable]
        notfree = Set.toAscList (Set.fromAscList [A .. ] \\ freeVars t)

        convert :: Int -> [Variable] -> [Variable] -> DeBruijn -> Term
        convert d bound fresh (DVar i) = if (d > i) then Var $ bound !! i
                                         else Var $ toEnum (i - d)
        convert d bound (f:fs) (DAbs t) = Abs f (convert (d+1) (f:bound) fs t) 
        convert d bound fresh (DApp t1 t2) = App (convert d bound fresh t1) (convert d bound fresh t2)
        
freeVars :: DeBruijn -> Set Variable
freeVars = getVars 0
    where 
        getVars :: Int -> DeBruijn -> Set Variable
        getVars d (DVar i) = if d > i then empty else singleton (toEnum $ i - d)
        getVars d (DAbs t) = getVars (d+1) t
        getVars d (DApp t1 t2) = getVars d t1 `union` getVars d t2

parseVariable :: Parser Variable
parseVariable =  (eat "a" >> return A)
        <|> (eat "b" >> return B)
        <|> (eat "c" >> return C)
        <|> (eat "d" >> return D)
        <|> (eat "e" >> return E)
        <|> (eat "f" >> return F)
        <|> (eat "g" >> return G)
        <|> (eat "h" >> return H)
        <|> (eat "i" >> return I)
        <|> (eat "j" >> return J)
        <|> (eat "k" >> return K)
        <|> (eat "l" >> return L)
        <|> (eat "m" >> return M)
        <|> (eat "n" >> return N)
        <|> (eat "o" >> return O)
        <|> (eat "p" >> return P)
        <|> (eat "q" >> return Q)
        <|> (eat "r" >> return R)
        <|> (eat "s" >> return S)
        <|> (eat "t" >> return T)
        <|> (eat "u" >> return U)
        <|> (eat "v" >> return V)
        <|> (eat "w" >> return W)
        <|> (eat "x" >> return X)
        <|> (eat "y" >> return Y)
        <|> (eat "z" >> return Z)

parseCombinator :: Parser Term
parseCombinator = (eat "I" >> return iComb)
        <|> (eat "S" >> return sComb)
        <|> (eat "K" >> return kComb)
        <|> (eat "Y" >> return yComb)
        <|> (eat "T" >> return tComb)
        <|> (eat "F" >> return fComb)
        <|> (eat "Ω" >> return omegaComb)

parseTerm :: Parser Term
parseTerm =  parseApp <|> parseNonApp
    where 
        parseVar = Var <$> parseVariable
        parseAbs = do
            eat "λ"
            vs <- some parseVariable
            eat "."
            t <- parseTerm
            return $ foldr Abs t vs 
        parseApp = chainl1 parseNonApp (return App)
        parseNonApp = parseVar <|> parseCombinator <|> parseAbs <|> parens parseTerm

bracketShow :: Term -> String
bracketShow (Var v) = show v
bracketShow (App t1 t2) = "(" ++ bracketShow t1 ++ bracketShow t2 ++ ")"
bracketShow (Abs v t) = "(λ" ++ show v ++ "." ++ bracketShow t ++ ")"